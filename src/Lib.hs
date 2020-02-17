module Lib
    ( mkEnv
    , runServer
    , main
    ) where

import System.FilePath
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (withMVar, modifyMVar_)
import qualified Data.HashMap.Strict as HashMap
import System.FSNotify

import Lib.App (Files(..),loadFiles, Env(..))
import Lib.Config (Config (..), loadConfig)

import Lib.Dump
import Lib.Doneshooting


mkEnv :: Config -> IO Env
mkEnv _ = do
    files <- newMVar =<< loadFiles "config.json"
    pure Env{..}


runServer :: Env -> IO ()
runServer env@Env{..} =
    withManager $ \mgr -> do
        watchers <- newMVar mempty

        pathDoneshooting mgr watchers env
        configDoneshooting mgr watchers env

        forever $ threadDelay 1000000


configDoneshooting :: WatchManager -> MVar (HashMap String (IO ())) -> Env -> IO ()
configDoneshooting mgr watchers Env{..} =
    withMVar files $ \ Files{..} -> do
        stop <- watchDir
            mgr
            (dropFileName doneshooting)
            (\e -> eventPath e == doneshooting)
            (\e -> withMVar watchers (HashMap.! "doneshooting"))
        return ()


pathDoneshooting :: WatchManager -> MVar (HashMap String (IO ())) -> Env -> IO ()
pathDoneshooting mgr watchers env@Env{..} =
    withMVar files $ \ Files{..} -> do
        (Doneshooting path) <- getDoneshooting doneshooting
        stop <- watchDir
            mgr
            path
            (const True)
            print

        modifyMVar_ watchers $ return
                             . HashMap.insert "doneshooting"
                             (stop >> pathDoneshooting mgr watchers env)


main :: IO ()
main = loadConfig >>= mkEnv >>= runServer
