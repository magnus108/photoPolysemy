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

import Lib.Doneshooting

import qualified Lib.Server.Server as Server

import Graphics.UI.Threepenny (newEvent, Handler)


mkEnv :: Config -> IO Env
mkEnv _ = do
    files <- newMVar =<< loadFiles "config.json"
    pure Env{..}


runServer :: Env -> IO ()
runServer env@Env{..} = do
    (eDoneshooting, hDoneshooting) <- newEvent

    withManager $ \mgr -> do
        watchers <- newMVar mempty

        pathDoneshooting mgr watchers env hDoneshooting
        configDoneshooting mgr watchers env

        Server.run env eDoneshooting hDoneshooting


configDoneshooting :: WatchManager -> MVar (HashMap String (IO ())) -> Env -> IO ()
configDoneshooting mgr watchers Env{..} =
    withMVar files $ \ Files{..} -> do
        _ <- watchDir
            mgr
            (dropFileName doneshooting)
            (\e -> eventPath e == doneshooting)
            (\_ -> withMVar watchers (HashMap.! "doneshooting"))
        return ()


pathDoneshooting :: WatchManager -> MVar (HashMap String (IO ())) -> Env -> Handler String -> IO ()
pathDoneshooting mgr watchers env@Env{..} handler =
    withMVar files $ \ Files{..} -> do
        (Doneshooting path) <- getDoneshooting doneshooting
        stop <- watchDir
            mgr
            path
            (const True)
            (\e -> print e >> handler (show e))

        modifyMVar_ watchers $ return
                             . HashMap.insert "doneshooting"
                             (stop >> pathDoneshooting mgr watchers env handler)


main :: IO ()
main = loadConfig >>= mkEnv >>= runServer
