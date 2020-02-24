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

import Lib.Doneshooting

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
    (eTab, hTab) <- newEvent

    watchers <- newMVar mempty
    withManager $ \mgr -> do
        withMVar files $ \ files' -> do
            --Tabs
            stopConfigTab <- configTab mgr files' watchers hTab

            --Doneshooting
            stopConfigDoneshooting <- configDoneshooting mgr files' watchers hConfigDoneshooting hDirDoneshooting
            stopDirDoneshooting <- dirDoneshooting mgr files' watchers hDirDoneshooting

            --TODO setter
            modifyMVar_ watchers $ \_ -> do
                return $ HashMap.fromList
                    [("configTab", stopConfigTab )
                    ,("stopConfigDoneshooting", stopConfigDoneshooting)
                    ,("stopDirDoneshooting", stopDirDoneshooting)
                    ]

        --VERY important this is here
        Server.run port env eConfigDoneshooting eTab


type WatchMap = MVar (HashMap String StopListening)


configTab :: WatchManager -> Files -> WatchMap -> Handler Tabs -> IO StopListening
configTab mgr Files{..} _ handler = watchDir
        mgr
        (dropFileName tabsFile)
        (\e -> eventPath e == tabsFile)
        (\e -> print e >> (handler =<< getTabs tabsFile))


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



main :: Int -> IO ()
main port = loadConfig >>= mkEnv >>= (runServer port)
