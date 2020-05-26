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

import Lib.App (Files(..), Env(..))
import Lib.Config (Config (..), loadConfig)
import Lib.Tab (Tabs, getTabs)
import Lib.Photographer (getPhotographers)

import Utils.ListZipper

import qualified Lib.Translation as Translation
import Lib.Data
import Lib.Grade (Grades, getGrades, writeGrades, Grade(..), Grades(..))
import qualified Lib.Photographee as Photographee
import Lib.Location
import Lib.Session
import Lib.Shooting
import Lib.Camera
import Lib.Dagsdato
import Lib.DagsdatoBackup
import Lib.Doneshooting
import Lib.Dump
import qualified Lib.Grade as Grade
import qualified Lib.Session as Session
import qualified Lib.Shooting as Shooting
import qualified Lib.Dump as Dump
import qualified Lib.Dagsdato as Dagsdato
import qualified Lib.Location as Location
import qualified Lib.Doneshooting as Doneshooting
import qualified Lib.DagsdatoBackup as DagsdatoBackup
import qualified Lib.Photographer as Photographer
import qualified Lib.Camera as Camera

import qualified Lib.Server.Server as Server

import Graphics.UI.Threepenny (newEvent, Handler)


mkEnv :: Config -> IO Env
mkEnv Config{..} = do

    mPhotographersFile <- newMVar photographersFile
    mGradesFile <- newMVar gradesFile
    mDumpFile <- newMVar dumpFile
    mDagsdatoFile <- newMVar dagsdatoFile
    mDagsdatoBackupFile <- newMVar dagsdatoBackupFile
    mDoneshootingFile <- newMVar doneshootingFile
    mCamerasFile <- newMVar camerasFile
    mShootingsFile <- newMVar shootingsFile
    mSessionsFile <- newMVar sessionsFile
    mLocationConfigFile <- newMVar locationConfigFile

    mTranslationFile <- newMVar translationFile

    mPhotographeesFile <- newMVar photograheesFile

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

    (ePhotographees, hPhotographees) <- newEvent

    watchers <- newMVar mempty
    withManager $ \mgr -> do
        --Photographers
        stopConfigPhotographers <- configPhotographers mgr mPhotographersFile watchers hPhotographers

        --Grades
        stopGrades <- grades mgr mGradesFile mLocationConfigFile mPhotographeesFile watchers hGrades

        --Grades
        stopPhotographees <- photographees mgr mPhotographeesFile watchers hPhotographees

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

        --Sessions
        stopConfigSessions <- configSessions mgr mSessionsFile watchers hSessions

        --Location
        stopConfigLocationFile <- configLocationFile mgr mLocationConfigFile mGradesFile watchers hLocationConfigFile

        withMVar files $ \ files' -> do
            --Tabs
            stopConfigTab <- configTab mgr files' watchers hTab

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

        -- Sessions
        bSessions <- UI.stepper Session.initialState eSessions
        _ <- Session.getSessions mSessionsFile hSessions

        -- Grades
        -- Photographees this is REAL BAD AS it does not wrk with bLocationConfigFile as expected
        bPhotographees <- UI.stepper Photographee.initialState ePhotographees
        bGrades <- UI.stepper Grade.initialState eGrades
        _ <- Grade.getGrades mGradesFile hGrades


        -- Location
        bLocationConfigFile <- UI.stepper Location.initialState eLocationConfigFile
        _ <- Location.getLocationFile mLocationConfigFile hLocationConfigFile

        -- DumpDir
        bDumpDir <- UI.stepper Dump.initalStateDir eDumpDir
        _ <- Dump.getDumpDir mDumpFile hDumpDir


        translations <- Translation.read mTranslationFile
        --VERY important this is here.. BADNESS FIX AT THE END
        Server.run port env (fromJust (rightToMaybe translations)) bGrades bLocationConfigFile bSessions bShootings bCameras bDump bDumpDir bDoneshooting bDagsdato bDagsdatoBackup eTabs bPhotographers bPhotographees hGrades hLocationConfigFile hConfigDump hDumpDir hPhotographees


type WatchMap = MVar (HashMap String StopListening)


configTab :: WatchManager -> Files -> WatchMap -> Handler Tabs -> IO StopListening
configTab mgr Files{..} _ handler = watchDir
        mgr
        (dropFileName tabsFile)
        (\e -> eventPath e == tabsFile)
        (\e -> print e >> (handler =<< getTabs tabsFile))


configLocationFile :: WatchManager -> MVar FilePath -> MVar FilePath ->  WatchMap -> Handler Location.Model -> IO StopListening
configLocationFile mgr mLocationConfigFile mGradesFile _ handler = do
    filepath <- readMVar mLocationConfigFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> do
            --TODO madness of baddness
            print e
            filepath <- readMVar mLocationConfigFile
            locationFile <- Location.getLocationFile' filepath
            grades' <- mapM Photographee.parseGrades locationFile
            let grades'' = either (const (Grades (ListZipper [] (Grade "") []))) id (join grades')
            void $ writeGrades mGradesFile grades''
        )


-- der skal skydes et lag in herimellem der kan lytte på locationen
photographees :: WatchManager -> MVar FilePath -> WatchMap -> Handler Photographee.Model -> IO StopListening
photographees mgr mPhotographeesFile _ handler = do
    filepath <- readMVar mPhotographeesFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> void $ do
            print e
            Photographee.getPhotographees mPhotographeesFile handler
        )


-- der skal skydes et lag in herimellem der kan lytte på locationen
grades :: WatchManager -> MVar FilePath -> MVar FilePath -> MVar FilePath -> WatchMap -> Handler Grade.Model -> IO StopListening
grades mgr mGradesFile mLocationConfigFile mPhotographeesFile _ handler = do
    filepath <- readMVar mGradesFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> void $ do
            print e 
            getGrades mGradesFile handler
            Photographee.reloadPhotographees mGradesFile mLocationConfigFile mPhotographeesFile
        )


configPhotographers :: WatchManager -> MVar FilePath -> WatchMap -> Handler Photographer.Model -> IO StopListening
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


configSessions :: WatchManager -> MVar FilePath -> WatchMap -> Handler Session.Model -> IO StopListening
configSessions mgr mSessionsFile _ handler = do
    filepath <- readMVar mSessionsFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> void $ print e >> getSessions mSessionsFile handler)


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


configDump :: WatchManager -> MVar FilePath -> WatchMap -> Handler (Dump.DumpModel) -> Handler (Dump.DumpDirModel) -> IO StopListening
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


dirDump :: WatchManager -> MVar FilePath -> WatchMap -> Handler Dump.DumpDirModel -> IO StopListening
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
            (\e -> void $ print e >> getDumpDir mDump handler)


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
main port = loadConfig "config.json" >>= mkEnv >>= runServer port
