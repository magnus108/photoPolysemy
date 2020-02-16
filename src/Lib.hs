module Lib
    ( mkEnv
    , runServer
    , main
    ) where

import System.FilePath
import Control.Concurrent (threadDelay)
import System.FSNotify

import Lib.App (Env(..))
import Lib.Config (Config (..), loadConfig)

import Lib.Dump
import Lib.Doneshooting

mkEnv :: Config -> IO Env
mkEnv Config{..} = do
    let _dump = getDump cDump -- skal watche _dump og cdump
    _doneshooting <- getDoneshooting cDoneshooting
    pure Env{..}


runServer :: Env -> IO ()
runServer env@Env{..} = do
    withManager $ \mgr -> do
        _ <- sDoneshooting mgr env
        forever $ threadDelay 1000000


sDoneshooting :: WatchManager -> Env -> IO StopListening
sDoneshooting mgr Env{..} = do
        stop <- watchTree --danger
            mgr
            "." -- has to be config dirsomehow
            (\e -> case _doneshooting of
                     NoDoneshooting -> False
                     YesDoneshooting doneshootingDir _ -> dropFileName (eventPath e) == doneshootingDir
            )
            print

        return stop

main :: IO ()
main = loadConfig >>= mkEnv >>= runServer
