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
import Lib.Photographer (getPhotographers)

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
import qualified Lib.Shooting as Shooting
import qualified Lib.Dump as Dump
import qualified Lib.Dagsdato as Dagsdato
import qualified Lib.Doneshooting as Doneshooting
import qualified Lib.DagsdatoBackup as DagsdatoBackup
import qualified Lib.Photographer as Photographer
import qualified Lib.Camera as Camera

import qualified Lib.Server.Server as Server

import Graphics.UI.Threepenny (newEvent, Handler)


mkEnv :: Config -> IO Env
mkEnv _ = do
    Files2{..} <- loadFiles "config.json"

    mPhotographersFile <- newMVar photographersFile
    mGradesFile <- newMVar gradesFile
    mDumpFile <- newMVar dumpFile
    mDagsdatoFile <- newMVar dagsdatoFile
    mDagsdatoBackupFile <- newMVar dagsdatoBackupFile
    mDoneshootingFile <- newMVar doneshootingFile
    mCamerasFile <- newMVar camerasFile
    mShootingsFile <- newMVar shootingsFile

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

        --Dump
        stopConfigDump <- configDump mgr mDumpFile watchers hConfigDump hDumpDir
        stopDirDump <- dirDump mgr mDumpFile watchers hDumpDir

        --Dagsdato
        stopConfigDagsdato <- configDagsdato mgr mDagsdatoFile watchers hConfigDagsdato hDirDagsdato
        stopDirDagsdato <- dirDagsdato mgr mDagsdatoFile watchers hDirDagsdato

        --Dagsdato backup
        stopConfigDagsdatoBackup <- configDagsdatoBackup mgr mDagsdatoBackupFile watchers hConfigDagsdatoBackup hDirDagsdatoBackup
        stopDirDagsdatoBackup <- dirDagsdatoBackup mgr mDagsdatoBackupFile watchers hDirDagsdatoBackup

        --Doneshooting
        stopConfigDoneshooting <- configDoneshooting mgr mDoneshootingFile watchers hConfigDoneshooting hDirDoneshooting
        stopDirDoneshooting <- dirDoneshooting mgr mDoneshootingFile watchers hDirDoneshooting

        --Cameras
        stopConfigCameras <- configCameras mgr mCamerasFile watchers hCameras

        --Shootings
        stopConfigShootings <- configShootings mgr mShootingsFile watchers hShootings

        withMVar files $ \ files' -> do
            --Tabs
            stopConfigTab <- configTab mgr files' watchers hTab

            --Location
            stopConfigLocationFile <- configLocationFile mgr files' watchers hLocationConfigFile

            --Sessions
            stopConfigSessions <- configSessions mgr files' watchers hSessions

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

        --Photographers
        bPhotographers <- UI.stepper Photographer.initalState ePhotographers
        _ <- getPhotographers mPhotographersFile hPhotographers

        --Dump
        bDump <- UI.stepper Dump.initalState eConfigDump
        _ <- Dump.getDump mDumpFile hConfigDump

        --Dagsdato
        bDagsdato <- UI.stepper Dagsdato.initialState eConfigDagsdato
        _ <- Dagsdato.getDagsdato mDagsdatoFile hConfigDagsdato

        --DagsdatoBackup
        bDagsdatoBackup <- UI.stepper DagsdatoBackup.initialState eConfigDagsdatoBackup
        _ <- DagsdatoBackup.getDagsdatoBackup mDagsdatoBackupFile hConfigDagsdatoBackup

        -- Doneshooting
        bDoneshooting <- UI.stepper Doneshooting.initialState eConfigDoneshooting
        _ <- Doneshooting.getDoneshooting mDoneshootingFile hConfigDoneshooting

        -- Cameras
        bCameras <- UI.stepper Camera.initalState eCameras
        _ <- Camera.getCameras mCamerasFile hCameras

        -- Shootings
        bShootings <- UI.stepper Shooting.initialState eShootings
        _ <- Shooting.getShootings mShootingsFile hShootings

        translations <- Translation.read mTranslationFile
        --VERY important this is here
        Server.run port env (fromJust (rightToMaybe translations)) eGrades eLocationConfigFile eSessions bShootings bCameras bDump eDumpDir bDoneshooting bDagsdato bDagsdatoBackup eTabs bPhotographers


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


configCameras :: WatchManager -> MVar FilePath -> WatchMap -> Handler Camera.Model -> IO StopListening
configCameras mgr mCamerasFile _ handler = do
    filepath <- readMVar mCamerasFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> void $ print e >> getCameras mCamerasFile handler)


configShootings :: WatchManager -> MVar FilePath -> WatchMap -> Handler Shooting.Model -> IO StopListening
configShootings mgr mShootingsFile _ handler = do
    filepath <- readMVar mShootingsFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> void $ print e >> getShootings mShootingsFile handler)


configDoneshooting :: WatchManager -> MVar FilePath -> WatchMap -> Handler Doneshooting.Model -> Handler () -> IO StopListening
configDoneshooting mgr mDoneshootingFile watchMap handler handleDonshootingDir = do
    filepath <- readMVar mDoneshootingFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> do
            print e
            _ <- Doneshooting.getDoneshooting mDoneshootingFile handler
            -- TODO these two are related
            modifyMVar_ watchMap $ \ h -> do
                h HashMap.! "stopDirDoneshooting"
                stopDirDoneshooting <- dirDoneshooting mgr mDoneshootingFile watchMap handleDonshootingDir
                return $ HashMap.insert "stopDirDoneshooting" stopDirDoneshooting  h
        )



dirDoneshooting :: WatchManager -> MVar FilePath -> WatchMap -> Handler () -> IO StopListening
dirDoneshooting mgr mDoneshootingFile _ handler = do
    doneshootingFile <- readMVar mDoneshootingFile
    doneshootingPath <- getDoneshooting' doneshootingFile
    case doneshootingPath of
        Left _ -> return ( return ())
        Right path -> do
            watchDir
                mgr
                (unDoneshooting path)
                (const True)
                (\e -> print e >> handler ())
                    `catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks


configDump :: WatchManager -> MVar FilePath -> WatchMap -> Handler (Dump.DumpModel) -> Handler (Data String DumpDir) -> IO StopListening
configDump mgr mDumpFile watchMap handler handleDumpDir = do
    filepath <- readMVar mDumpFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        --TODO SPAWNER THREAD SOM IKKE DØR
        --TODO SPAWNER THREAD SOM IKKE DØR
        --TODO SPAWNER THREAD SOM IKKE DØR
        (\e -> do
            print e
            _ <- Dump.getDump mDumpFile handler
            modifyMVar_ watchMap $ \ h -> do
                h HashMap.! "stopDirDump"
                stopDirDump <- dirDump mgr mDumpFile watchMap handleDumpDir
                return $ HashMap.insert "stopDirDump" stopDirDump h
        )


dirDump :: WatchManager -> MVar FilePath -> WatchMap -> Handler (Data String DumpDir) -> IO StopListening
dirDump mgr mDump _ handler = do
    dumpFile <- readMVar mDump
    dumpPath <- getDump' dumpFile
    case dumpPath of
      Left _ -> return (return ()) -- TODO this sucks
      Right path -> do
        watchDir
            mgr
            (unDump path)
            (const True)
            (\e -> print e >> (void $ getDumpDir (unDump path) handler))


configDagsdato :: WatchManager -> MVar FilePath -> WatchMap -> Handler (Dagsdato.Model) -> Handler () -> IO StopListening
configDagsdato mgr mDagsdatoFile watchMap handler handleDagsdatoDir = do
    filepath <- readMVar mDagsdatoFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> do
            print e
            _ <- Dagsdato.getDagsdato mDagsdatoFile handler
            modifyMVar_ watchMap $ \ h -> do
                h HashMap.! "stopDirDagsdato"
                stopDirDagsdato <- dirDagsdato mgr mDagsdatoFile watchMap handleDagsdatoDir
                return $ HashMap.insert "stopDirDagsdato" stopDirDagsdato h
        )




dirDagsdato :: WatchManager -> MVar FilePath -> WatchMap -> Handler () -> IO StopListening
dirDagsdato mgr mDagsdatoFile _ handler = do
    dagsdatoFile <- readMVar mDagsdatoFile
    dagsdatoPath <- getDagsdato' dagsdatoFile
    case dagsdatoPath of
        Left _ -> return (return ())
        Right path -> 
            watchDir
                mgr
                (unDagsdato path)
                (const True)
                (\e -> print e >> handler ())
                    `catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks


configDagsdatoBackup :: WatchManager -> MVar FilePath -> WatchMap -> Handler DagsdatoBackup.Model -> Handler () -> IO StopListening
configDagsdatoBackup mgr mDagsdatoBackupFile watchMap handler handleDagsdatoBackupDir = do
    filepath <- readMVar mDagsdatoBackupFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> do
            print e
            _ <- DagsdatoBackup.getDagsdatoBackup mDagsdatoBackupFile handler
            modifyMVar_ watchMap $ \ h -> do
                h HashMap.! "stopDirDagsdatoBackup"
                stopDirDagsdatoBackup <- dirDagsdatoBackup mgr mDagsdatoBackupFile watchMap handleDagsdatoBackupDir
                return $ HashMap.insert "stopDirDagsdatoBackup" stopDirDagsdatoBackup h
        )



dirDagsdatoBackup :: WatchManager -> MVar FilePath -> WatchMap -> Handler () -> IO StopListening
dirDagsdatoBackup mgr mDagsdatoBackupFile _ handler = do
    dagsdatoBackupFile <- readMVar mDagsdatoBackupFile
    dagsdatoBackupPath <- getDagsdatoBackup' dagsdatoBackupFile
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
