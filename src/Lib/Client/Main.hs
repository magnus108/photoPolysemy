module Lib.Client.Main
    ( mainSection
    ) where


import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Tab
import Lib.Dump
import Lib.Client.Tab
import Lib.Client.Utils

import Lib.App (Env(..), Files(..))
import Control.Concurrent.MVar


mainSection :: Env -> Window -> Tabs -> Event (Either String Dump) -> Event (Either String DumpDir) -> UI ()
mainSection env@Env{..} win tabs eDump eDumpDir = do

    dump <- liftIO $ withMVar files $ \ Files{..} -> getDump dumpFile
    -- TODO this dump
    dumpDir <- liftIO $ withMVar files $ \ Files{..} -> do
        let filePath = unDump <$> dump
        dumpDir <- mapM getDumpDir filePath
        return $ join dumpDir

    case dumpDir of
        Left _ -> do
                tabs' <- mkTabs env tabs
                navigation <- mkNavigation env tabs
                view <- UI.div #+ fmap element
                    [ tabs'
                    , navigation
                    ]

                void $ UI.getBody win # set children [view]

        Right dumpDir' -> do
                let (_, eDumpDirSucc) = split eDumpDir

                bDumpDir <- stepper dumpDir' eDumpDirSucc

                _ <- stepper dump eDump

                count <- UI.div # sink item ((string . show . length . unDumpDir) <$> bDumpDir)


                tabs' <- mkTabs env tabs
                navigation <- mkNavigation env tabs
                view <- UI.div #+ fmap element
                    [ tabs'
                    , count
                    , navigation
                    ]

                void $ UI.getBody win # set children [view]
