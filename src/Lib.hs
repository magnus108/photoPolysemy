{-# LANGUAGE ScopedTypeVariables #-}
module Lib
    ( mkEnv
    , runServer
    , main
    ) where

import Relude.Unsafe (fromJust)

import qualified Graphics.UI.Threepenny as UI

import Control.Exception (SomeException(..), catch)

import System.FilePath
import Control.Concurrent.MVar (withMVar, modifyMVar_)
import qualified Data.HashMap.Strict as HashMap
import System.FSNotify

import Lib.App (Files(..),loadFiles, Env(..))
import Lib.App.Files2
import Lib.Config (Config (..), loadConfig)
import Lib.Tab (Tabs, getTabs)
import Lib.Photographer (Photographers, getPhotographers)

import Utils.ListZipper

import qualified Lib.Translation as Translation
import Lib.Data
import Lib.Grade (Grades, getGrades, writeGrades, Grade(..), Grades(..), parseGrades)
import Lib.Location
import Lib.Session
import Lib.Shooting
import Lib.Camera
import Lib.Dagsdato
import Lib.DagsdatoBackup
import Lib.Doneshooting
import Lib.Dump
import qualified Lib.Photographer as Photographer

import qualified Lib.Server.Server as Server

import Graphics.UI.Threepenny (newEvent, Handler)


mkEnv :: Config -> IO Env
mkEnv _ = do
    Files2{..} <- loadFiles "config.json"

    mPhotographersFile <- newMVar photographersFile
    mGradesFile <- newMVar gradesFile
    mTranslationFile <- newMVar translationFile

    files <- newMVar Files{..}
    pure Env{..}


runServer :: Int -> Env -> IO ()
runServer port env@Env{..} = do
    (_, hDirDoneshooting) <- newEvent
    (eConfigDoneshooting, hConfigDoneshooting) <- newEvent

    (_, hDirDagsdato) <- newEvent
    (eConfigDagsdato, hConfigDagsdato) <- newEvent

    (_, hDirDagsdatoBackup) <- newEvent
    (eConfigDagsdatoBackup, hConfigDagsdatoBackup) <- newEvent

    (eDumpDir, hDumpDir) <- newEvent
    (eConfigDump, hConfigDump) <- newEvent

    (eTabs, hTab) <- newEvent
    (ePhotographers, hPhotographers) <- newEvent

    (eCameras, hCameras) <- newEvent

    (eShootings, hShootings) <- newEvent

    (eSessions, hSessions) <- newEvent

    (eGrades, hGrades) <- newEvent
    (eLocationConfigFile, hLocationConfigFile) <- newEvent

    watchers <- newMVar mempty
    withManager $ \mgr -> do
        --Photographers
        stopConfigPhotographers <- configPhotographers mgr mPhotographersFile watchers hPhotographers

        --Grades
        stopGrades <- grades mgr mGradesFile watchers hGrades

        withMVar files $ \ files' -> do
            --Tabs
            stopConfigTab <- configTab mgr files' watchers hTab

            --Location
            stopConfigLocationFile <- configLocationFile mgr files' watchers hLocationConfigFile


            --Sessions
            stopConfigSessions <- configSessions mgr files' watchers hSessions

            --Cameras
            stopConfigCameras <- configCameras mgr files' watchers hCameras

            --Cameras
            stopConfigShootings <- configShootings mgr files' watchers hShootings

            --Doneshooting
            stopConfigDoneshooting <- configDoneshooting mgr files' watchers hConfigDoneshooting hDirDoneshooting
            stopDirDoneshooting <- dirDoneshooting mgr files' watchers hDirDoneshooting

            --Dagsdato
            stopConfigDagsdato <- configDagsdato mgr files' watchers hConfigDagsdato hDirDagsdato
            stopDirDagsdato <- dirDagsdato mgr files' watchers hDirDagsdato

            --Dagsdato backup
            stopConfigDagsdatoBackup <- configDagsdatoBackup mgr files' watchers hConfigDagsdatoBackup hDirDagsdatoBackup
            stopDirDagsdatoBackup <- dirDagsdatoBackup mgr files' watchers hDirDagsdatoBackup

            --Dump
            stopConfigDump <- configDump mgr files' watchers hConfigDump hDumpDir
            stopDirDump <- dirDump mgr files' watchers hDumpDir

            --TODO setter
            modifyMVar_ watchers $ \_ -> do
                return $ HashMap.fromList
                    [("stopConfigTab", stopConfigTab )

                    ,("stopConfigLocationFile", stopConfigLocationFile)
                    ,("stopGrades", stopGrades)

                    ,("stopConfigPhotographers", stopConfigPhotographers)

                    ,("stopConfigCameras", stopConfigCameras)

                    ,("stopConfigShootings", stopConfigShootings)

                    ,("stopConfigSessions", stopConfigSessions)
                    
                    ,("stopConfigDoneshooting", stopConfigDoneshooting)
                    ,("stopDirDoneshooting", stopDirDoneshooting)

                    ,("stopConfigDagsdato", stopConfigDagsdato)
                    ,("stopDirDagsdato", stopDirDagsdato)

                    ,("stopConfigDagsdatoBackup", stopConfigDagsdatoBackup)
                    ,("stopDirDagsdatoBackup", stopDirDagsdatoBackup)

                    ,("stopConfigDump", stopConfigDump)
                    ,("stopDirDump", stopDirDump)
                    ]

        bPhotographers <- UI.stepper Photographer.initalState ePhotographers
        _ <- getPhotographers mPhotographersFile hPhotographers

        translations <- Translation.read mTranslationFile
        --VERY important this is here
        Server.run port env (fromJust (rightToMaybe translations)) eGrades eLocationConfigFile eSessions eShootings eCameras eConfigDump eDumpDir eConfigDoneshooting eConfigDagsdato eConfigDagsdatoBackup eTabs bPhotographers


type WatchMap = MVar (HashMap String StopListening)


configTab :: WatchManager -> Files -> WatchMap -> Handler Tabs -> IO StopListening
configTab mgr Files{..} _ handler = watchDir
        mgr
        (dropFileName tabsFile)
        (\e -> eventPath e == tabsFile)
        (\e -> print e >> (handler =<< getTabs tabsFile))


configLocationFile :: WatchManager -> Files -> WatchMap -> Handler (Either String LocationFile) -> IO StopListening
configLocationFile mgr Files{..} _ handler = watchDir
        mgr
        (dropFileName locationConfigFile)
        (\e -> eventPath e == locationConfigFile)
        (\e -> do
            print e
            locationFile <- getLocationFile locationConfigFile
            handler locationFile
            grades' <- mapM parseGrades locationFile
            let grades'' = either (const (Grades (ListZipper [] (Grade "") []))) id (join grades')
            writeGrades gradesFile grades''
        )

-- der skal skydes et lag in herimellem der kan lytte på locationen

grades :: WatchManager -> MVar FilePath -> WatchMap -> Handler (Data String Grades) -> IO StopListening
grades mgr mFilepath _ handler = do
    filepath <- readMVar mFilepath
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> void $ print e >> getGrades mFilepath handler )


configPhotographers :: WatchManager -> MVar FilePath -> WatchMap -> Handler (Photographer.Model) -> IO StopListening
configPhotographers mgr mFilepath _ handler = do
    filepath <- readMVar mFilepath
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        --TODO SPAWNER THREAD SOM IKKE DØR
        --TODO SPAWNER THREAD SOM IKKE DØR
        --TODO SPAWNER THREAD SOM IKKE DØR
        (\e -> void $ print e >> getPhotographers mFilepath handler)


configSessions :: WatchManager -> Files -> WatchMap -> Handler (Either String Sessions) -> IO StopListening
configSessions mgr Files{..} _ handler = watchDir
        mgr
        (dropFileName sessionsFile)
        (\e -> eventPath e == sessionsFile)
        (\e -> print e >> (handler =<< getSessions sessionsFile))


configCameras :: WatchManager -> Files -> WatchMap -> Handler (Either String Cameras) -> IO StopListening
configCameras mgr Files{..} _ handler = watchDir
        mgr
        (dropFileName camerasFile)
        (\e -> eventPath e == camerasFile)
        (\e -> print e >> (handler =<< getCameras camerasFile))


configShootings :: WatchManager -> Files -> WatchMap -> Handler (Either String Shootings) -> IO StopListening
configShootings mgr Files{..} _ handler = watchDir
        mgr
        (dropFileName shootingsFile)
        (\e -> eventPath e == shootingsFile)
        (\e -> print e >> (handler =<< getShootings shootingsFile))


configDoneshooting :: WatchManager -> Files -> WatchMap -> Handler (Either String Doneshooting) -> Handler () -> IO StopListening
configDoneshooting mgr files@Files{..} watchMap handler handleDonshootingDir = watchDir
        mgr
        (dropFileName doneshootingFile)
        (\e -> eventPath e == doneshootingFile)
        (\e -> do
            print e
            handler =<< getDoneshooting doneshootingFile

            -- TODO these two are related
            modifyMVar_ watchMap $ \ h -> do
                h HashMap.! "stopDirDoneshooting"
                stopDirDoneshooting <- dirDoneshooting mgr files watchMap handleDonshootingDir
                return $ HashMap.insert "stopDirDoneshooting" stopDirDoneshooting  h
        )



dirDoneshooting :: WatchManager -> Files -> WatchMap -> Handler () -> IO StopListening
dirDoneshooting mgr Files{..} _ handler = do
    doneshootingPath <- getDoneshooting doneshootingFile
    case doneshootingPath of
        Left _ -> return ( return ())
        Right path -> do
            watchDir
                mgr
                (unDoneshooting path)
                (const True)
                (\e -> print e >> handler ())
                    `catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks


configDump :: WatchManager -> Files -> WatchMap -> Handler (Either String Dump) -> Handler (Data String DumpDir) -> IO StopListening
configDump mgr files@Files{..} watchMap handler handleDumpDir = watchDir
        mgr
        (dropFileName dumpFile)
        (\e -> eventPath e == dumpFile)
        (\e -> do
            print e
            handler =<< getDump' dumpFile
            -- TODO these two are related
            modifyMVar_ watchMap $ \ h -> do
                h HashMap.! "stopDirDump"
                stopDirDump <- dirDump mgr files watchMap handleDumpDir
                return $ HashMap.insert "stopDirDump" stopDirDump h
        )



dirDump :: WatchManager -> Files -> WatchMap -> Handler (Data String DumpDir) -> IO StopListening
dirDump mgr Files{..} _ handler = do
    dumpPath <- getDump' dumpFile
    case dumpPath of
      Left _ -> return (return ()) -- TODO this sucks
      Right path -> do
        watchDir
            mgr
            (unDump path)
            (const True)
            (\e -> print e >> (void $ getDumpDir (unDump path) handler))


configDagsdato :: WatchManager -> Files -> WatchMap -> Handler (Either String Dagsdato) -> Handler () -> IO StopListening
configDagsdato mgr files@Files{..} watchMap handler handleDagsdatoDir = watchDir
        mgr
        (dropFileName dagsdatoFile)
        (\e -> eventPath e == dagsdatoFile)
        (\e -> do
            print e
            handler =<< getDagsdato dagsdatoFile

            -- TODO these two are related
            modifyMVar_ watchMap $ \ h -> do
                h HashMap.! "stopDirDagsdato"
                stopDirDagsdato <- dirDagsdato mgr files watchMap handleDagsdatoDir
                return $ HashMap.insert "stopDirDagsdato" stopDirDagsdato h
        )



dirDagsdato :: WatchManager -> Files -> WatchMap -> Handler () -> IO StopListening
dirDagsdato mgr Files{..} _ handler = do
    dagsdatoPath <- getDagsdato dagsdatoFile
    case dagsdatoPath of
        Left _ -> return (return ())
        Right path -> 
            watchDir
                mgr
                (unDagsdato path)
                (const True)
                (\e -> print e >> handler ())
                    `catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks



configDagsdatoBackup :: WatchManager -> Files -> WatchMap -> Handler (Either String DagsdatoBackup) -> Handler () -> IO StopListening
configDagsdatoBackup mgr files@Files{..} watchMap handler handleDagsdatoBackupDir = watchDir
        mgr
        (dropFileName dagsdatoBackupFile)
        (\e -> eventPath e == dagsdatoBackupFile)
        (\e -> do
            print e
            handler =<< getDagsdatoBackup dagsdatoBackupFile

            -- TODO these two are related
            modifyMVar_ watchMap $ \ h -> do
                h HashMap.! "stopDirDagsdatoBackup"
                stopDirDagsdatoBackup <- dirDagsdatoBackup mgr files watchMap handleDagsdatoBackupDir
                return $ HashMap.insert "stopDirDagsdatoBackup" stopDirDagsdatoBackup h
        )



dirDagsdatoBackup :: WatchManager -> Files -> WatchMap -> Handler () -> IO StopListening
dirDagsdatoBackup mgr Files{..} _ handler = do
    dagsdatoBackupPath <- getDagsdatoBackup dagsdatoBackupFile
    case dagsdatoBackupPath of
        Left _ -> return (return ()) -- TODO this sucks
        Right path -> watchDir
            mgr
            (unDagsdatoBackup path)
            (const True)
            (\e -> print e >> handler ())
                `catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks


main :: Int -> IO ()
main port = loadConfig >>= mkEnv >>= runServer port
