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

import Lib.App (Env(..))
import Lib.Config (Config (..), loadConfig)

import Lib.Dump
import Lib.Doneshooting

mkEnv :: Config -> IO Env
mkEnv config@Config{..} = do
    dump <- newMVar =<< getDump cDump -- skal watche _dump og cdump
    doneshooting <- newMVar =<< getDoneshooting cDoneshooting

    configDump <- newMVar cDump
    configDoneshooting <- newMVar cDoneshooting

    pure Env{..}


runServer :: Env -> IO ()
runServer env@Env{..} = do
    withManager $ \mgr -> do
        watchers <- newMVar mempty

        sDoneshooting mgr watchers env
        sConfigDoneshooting mgr watchers env

        forever $ threadDelay 1000000


sConfigDoneshooting :: WatchManager -> MVar (HashMap String (IO ())) -> Env -> IO ()
sConfigDoneshooting mgr watchers Env{..} = do
    filepath <- readMVar configDoneshooting
    stop <- watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> withMVar watchers (HashMap.! "doneshooting"))
    return ()


sDoneshooting :: WatchManager -> MVar (HashMap String (IO ())) -> Env -> IO ()
sDoneshooting mgr watchers env@Env{..} = do
    doneshooting' <- readMVar doneshooting
    case doneshooting' of
        NoDoneshooting -> return ()
        YesDoneshooting doneshootingDir _ ->  do
            stop <- watchDir
                mgr
                doneshootingDir
                (const True)
                print

            modifyMVar_ watchers (return . HashMap.insert "doneshooting" (stop >> sDoneshooting mgr watchers env))
            return ()


main :: IO ()
main = loadConfig "config.json" >>= mkEnv >>= runServer
