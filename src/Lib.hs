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


runServer :: Env -> IO ()
runServer env@Env{..} = do
    (_, hDoneshooting) <- newEvent
    (eConfigDoneshooting, hConfigDoneshooting) <- newEvent
    (eTab, hTab) <- newEvent

    withManager $ \mgr -> do
        watchers <- newMVar mempty

        --Tabs
        configTab mgr watchers env hTab

        --Doneshooting
        pathDoneshooting mgr watchers env hDoneshooting
        configDoneshooting mgr watchers env hConfigDoneshooting

        Server.run env eConfigDoneshooting eTab


configTab :: WatchManager -> MVar (HashMap String (IO ())) -> Env -> Handler Tabs -> IO ()
configTab mgr _ Env{..} handler =
    withMVar files $ \ Files{..} -> do
        _ <- watchDir
            mgr
            (dropFileName tabsFile)
            (\e -> eventPath e == tabsFile)
            (\_ -> handler =<< getTabs tabsFile
            )
        return ()


configDoneshooting :: WatchManager -> MVar (HashMap String (IO ())) -> Env -> Handler Doneshooting -> IO ()
configDoneshooting mgr watchers Env{..} handler =
    withMVar files $ \ Files{..} -> do
        _ <- watchDir
            mgr
            (dropFileName doneshootingFile)
            (\e -> eventPath e == doneshootingFile)
            (\_ -> do
                --TODO should be transactional
                handler =<< getDoneshooting doneshootingFile
                withMVar watchers (HashMap.! "pathDoneshooting")
            )
        return ()


pathDoneshooting :: WatchManager -> MVar (HashMap String (IO ())) -> Env -> Handler () -> IO ()
pathDoneshooting mgr watchers env@Env{..} handler =
    withMVar files $ \ Files{..} -> do
        (Doneshooting path) <- getDoneshooting doneshootingFile
        stop <- watchDir
            mgr
            path
            (const True)
            (\e -> print e >> handler ())

        modifyMVar_ watchers $ return
                             . HashMap.insert "pathDoneshooting"
                             (stop >> pathDoneshooting mgr watchers env handler)


main :: IO ()
main = loadConfig >>= mkEnv >>= runServer
