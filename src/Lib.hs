module Lib
    ( mkEnv
    , runServer
    , main
    ) where

import System.FilePath
import Control.Concurrent.MVar (withMVar, modifyMVar_)
import qualified Data.HashMap.Strict as HashMap
import System.FSNotify

import Lib.App (Files(..),loadFiles, Env(..))
import Lib.Config (Config (..), loadConfig)
import Lib.Tab (Tabs, getTabs)
import Lib.Photographer (Photographers, getPhotographers)

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

    (_, hDirDump) <- newEvent
    (eConfigDump, hConfigDump) <- newEvent

    (eTabs, hTab) <- newEvent
    (ePhotographers, hPhotographer) <- newEvent

    watchers <- newMVar mempty
    withManager $ \mgr -> do
        withMVar files $ \ files' -> do
            --Tabs
            stopConfigTab <- configTab mgr files' watchers hTab
            --
            --Photographers
            stopConfigPhotographer <- configPhotographer mgr files' watchers hPhotographer

            --Doneshooting
            stopConfigDoneshooting <- configDoneshooting mgr files' watchers hConfigDoneshooting hDirDoneshooting
            stopDirDoneshooting <- dirDoneshooting mgr files' watchers hDirDoneshooting

            --Dump
            stopConfigDump <- configDump mgr files' watchers hConfigDump hDirDump
            stopDirDump <- dirDump mgr files' watchers hDirDump

            --TODO setter
            modifyMVar_ watchers $ \_ -> do
                return $ HashMap.fromList
                    [("configTab", stopConfigTab )
                    ,("configPhotographer", stopConfigPhotographer)
                    ,("stopConfigDoneshooting", stopConfigDoneshooting)
                    ,("stopDirDoneshooting", stopDirDoneshooting)
                    ,("stopConfigDump", stopConfigDump)
                    ,("stopDirDump", stopDirDump)
                    ]

        --VERY important this is here
        Server.run port env eConfigDump eConfigDoneshooting eTabs ePhotographers


type WatchMap = MVar (HashMap String StopListening)


configTab :: WatchManager -> Files -> WatchMap -> Handler Tabs -> IO StopListening
configTab mgr Files{..} _ handler = watchDir
        mgr
        (dropFileName tabsFile)
        (\e -> eventPath e == tabsFile)
        (\e -> print e >> (handler =<< getTabs tabsFile))


configPhotographer :: WatchManager -> Files -> WatchMap -> Handler Photographers -> IO StopListening
configPhotographer mgr Files{..} _ handler = watchDir
        mgr
        (dropFileName photographersFile)
        (\e -> eventPath e == photographersFile)
        (\e -> print e >> (handler =<< getPhotographers photographersFile))


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


main :: Int -> IO ()
main port = loadConfig >>= mkEnv >>= runServer port
