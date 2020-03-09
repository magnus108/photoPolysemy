module Lib
    ( mkEnv
    , runServer
    , main
    ) where

import Relude.Unsafe (fromJust)
import System.FilePath
import Control.Concurrent.MVar (withMVar, modifyMVar_)
import qualified Data.HashMap.Strict as HashMap
import System.FSNotify

import Lib.App (Files(..),loadFiles, Env(..))
import Lib.Config (Config (..), loadConfig)
import Lib.Tab (Tabs, getTabs)
import Lib.Photographer (Photographers, getPhotographers)

import Lib.Grade
import Lib.Location
import Lib.Session
import Lib.Shooting
import Lib.Camera
import Lib.Dagsdato
import Lib.DagsdatoBackup
import Lib.Doneshooting
import Lib.Dump

import qualified Lib.Server.Server as Server

import Graphics.UI.Threepenny (newEvent, Handler)


mkEnv :: Config -> IO Env
mkEnv _ = do
    files <- newMVar =<< loadFiles "config.json"
    pure Env{..}


runServer :: Int -> Env -> IO ()
runServer port env@Env{..} = do
    (_, hDirDoneshooting) <- newEvent
    (eConfigDoneshooting, hConfigDoneshooting) <- newEvent

    (_, hDirDagsdato) <- newEvent
    (eConfigDagsdato, hConfigDagsdato) <- newEvent

    (_, hDirDagsdatoBackup) <- newEvent
    (eConfigDagsdatoBackup, hConfigDagsdatoBackup) <- newEvent

    (_, hDirDump) <- newEvent
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
        withMVar files $ \ files' -> do
            --Tabs
            stopConfigTab <- configTab mgr files' watchers hTab

            --Location
            stopConfigLocationFile <- configLocationFile mgr files' watchers hLocationConfigFile hGrades
            --Grades
            stopGrades <- grades mgr files' watchers hGrades

            --Photographers
            stopConfigPhotographers <- configPhotographers mgr files' watchers hPhotographers

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
            stopConfigDump <- configDump mgr files' watchers hConfigDump hDirDump
            stopDirDump <- dirDump mgr files' watchers hDirDump

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

        --VERY important this is here
        Server.run port env eGrades eLocationConfigFile eSessions eShootings eCameras eConfigDump eConfigDoneshooting eConfigDagsdato eConfigDagsdatoBackup eTabs ePhotographers


type WatchMap = MVar (HashMap String StopListening)


configTab :: WatchManager -> Files -> WatchMap -> Handler Tabs -> IO StopListening
configTab mgr Files{..} _ handler = watchDir
        mgr
        (dropFileName tabsFile)
        (\e -> eventPath e == tabsFile)
        (\e -> print e >> (handler =<< getTabs tabsFile))


configLocationFile :: WatchManager -> Files -> WatchMap -> Handler LocationFile -> Handler Grades -> IO StopListening
configLocationFile mgr files@Files{..} watchMap handler handleGrades = watchDir
        mgr
        (dropFileName locationConfigFile)
        (\e -> eventPath e == locationConfigFile)
        (\e -> do
            print e
            handler =<< getLocationFile locationConfigFile

            -- TODO these two are related
            modifyMVar_ watchMap $ \ h -> do
                h HashMap.! "stopGrades"
                stopGrades <- grades mgr files watchMap handleGrades
                return $ HashMap.insert "stopGrades" stopGrades h
        )


grades :: WatchManager -> Files -> WatchMap -> Handler Grades -> IO StopListening
grades mgr Files{..} _ handler = do
    location <- getLocationFile locationConfigFile
    watchDir
        mgr
        (dropFileName (unLocationFile location))
        (\e -> eventPath e == unLocationFile location)
        (\e -> do
            print e
            grades' <- parseGrades location
            handler $ fromJust grades'
        )


configPhotographers :: WatchManager -> Files -> WatchMap -> Handler Photographers -> IO StopListening
configPhotographers mgr Files{..} _ handler = watchDir
        mgr
        (dropFileName photographersFile)
        (\e -> eventPath e == photographersFile)
        (\e -> print e >> (handler =<< getPhotographers photographersFile))


configSessions :: WatchManager -> Files -> WatchMap -> Handler Sessions -> IO StopListening
configSessions mgr Files{..} _ handler = watchDir
        mgr
        (dropFileName sessionsFile)
        (\e -> eventPath e == sessionsFile)
        (\e -> print e >> (handler =<< getSessions sessionsFile))


configCameras :: WatchManager -> Files -> WatchMap -> Handler Cameras -> IO StopListening
configCameras mgr Files{..} _ handler = watchDir
        mgr
        (dropFileName camerasFile)
        (\e -> eventPath e == camerasFile)
        (\e -> print e >> (handler =<< getCameras camerasFile))


configShootings :: WatchManager -> Files -> WatchMap -> Handler Shootings -> IO StopListening
configShootings mgr Files{..} _ handler = watchDir
        mgr
        (dropFileName shootingsFile)
        (\e -> eventPath e == shootingsFile)
        (\e -> print e >> (handler =<< getShootings shootingsFile))


configDoneshooting :: WatchManager -> Files -> WatchMap -> Handler Doneshooting -> Handler () -> IO StopListening
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
    (Doneshooting path) <- getDoneshooting doneshootingFile
    watchDir
        mgr
        path
        (const True)
        (\e -> print e >> handler ())


configDump :: WatchManager -> Files -> WatchMap -> Handler Dump -> Handler () -> IO StopListening
configDump mgr files@Files{..} watchMap handler handleDumpDir = watchDir
        mgr
        (dropFileName dumpFile)
        (\e -> eventPath e == dumpFile)
        (\e -> do
            print e
            handler =<< getDump dumpFile

            -- TODO these two are related
            modifyMVar_ watchMap $ \ h -> do
                h HashMap.! "stopDirDump"
                stopDirDump <- dirDump mgr files watchMap handleDumpDir
                return $ HashMap.insert "stopDirDump" stopDirDump h
        )



dirDump :: WatchManager -> Files -> WatchMap -> Handler () -> IO StopListening
dirDump mgr Files{..} _ handler = do
    (Dump path) <- getDump dumpFile
    watchDir
        mgr
        path
        (const True)
        (\e -> print e >> handler ())


configDagsdato :: WatchManager -> Files -> WatchMap -> Handler Dagsdato -> Handler () -> IO StopListening
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
    (Dagsdato path) <- getDagsdato dagsdatoFile
    watchDir
        mgr
        path
        (const True)
        (\e -> print e >> handler ())


configDagsdatoBackup :: WatchManager -> Files -> WatchMap -> Handler DagsdatoBackup -> Handler () -> IO StopListening
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
    (DagsdatoBackup path) <- getDagsdatoBackup dagsdatoBackupFile
    watchDir
        mgr
        path
        (const True)
        (\e -> print e >> handler ())


main :: Int -> IO ()
main port = loadConfig >>= mkEnv >>= runServer port
