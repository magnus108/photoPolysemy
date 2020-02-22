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


type WatchMap = MVar (HashMap String (IO ()))

configTab :: WatchManager -> WatchMap -> Env -> Handler Tabs -> IO ()
configTab mgr watchers env@Env{..} handler = do
    withMVar files $ \ Files{..} -> do
        _ <- watchDir
            mgr
            (dropFileName tabsFile)
            (\e -> eventPath e == tabsFile)
            (\_ -> updateTab watchers env handler)
        return ()

updateTab :: WatchMap -> Env -> Handler Tabs -> IO ()
updateTab watchers Env{..} handler =
    --TODO should be transactional
    withMVar files $ \ Files{..} ->
        handler =<< getTabs tabsFile


updateConfigDoneShooting :: WatchMap -> Env -> Handler Doneshooting -> IO ()
updateConfigDoneShooting watchers Env{..} handler = do
    --TODO should be transactional
    withMVar files $ \ Files{..} -> do
        handler =<< getDoneshooting doneshootingFile
        --TODO is this dangerous
        actions <- readMVar watchers
        traceShowM "was here8"
        actions HashMap.! "pathDoneshooting"
        traceShowM "was here9"


configDoneshooting :: WatchManager -> WatchMap -> Env -> Handler Doneshooting -> IO ()
configDoneshooting mgr watchers env@Env{..} handler =
    withMVar files $ \ Files{..} -> do
        _ <- watchDir
            mgr
            (dropFileName doneshootingFile)
            (\e -> eventPath e == doneshootingFile)
            (\e -> print e >> updateConfigDoneShooting watchers env handler
            )
        return ()


pathDoneshooting :: WatchManager -> WatchMap -> Env -> Handler () -> IO ()
pathDoneshooting mgr watchers env@Env{..} handler = do
    traceShowM "was here15"
    withMVar files $ \ Files{..} -> do
        (Doneshooting path) <- getDoneshooting doneshootingFile
        stop <- watchDir
            mgr
            path
            (const True)
            (\e -> print e >> handler ())

        traceShowM "was here11"
        modifyMVar_ watchers $ return
                . HashMap.insert "pathDoneshooting"
                ( stop >> pathDoneshooting mgr watchers env handler)
        traceShowM "was here10"

main :: IO ()
main = loadConfig >>= mkEnv >>= runServer
