module Lib.Client.Dump
    ( dumpSection
    ) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Tab
import Lib.Dump
import Lib.Client.Tab
import Lib.Client.Element
import Lib.Client.Utils

import Lib.App (Env(..), Files(..))
import Control.Concurrent.MVar (withMVar)


dumpView :: Env -> Either String Dump -> UI Element
dumpView Env{..} = \case
    Left x -> do
        title_ <- UI.p # set text "Der er en fejl med dump mappe"

        picker <- UI.div #+
            [ mkFolderPicker "dumpPicker" "Vælg config folder" $ \folder ->
                when (folder /= "") $
                    withMVar files $ \ Files{..} ->
                        writeFile dumpFile (show folder)
            ]

        UI.div #+ fmap element [ title_, picker]

    Right dump -> do

        title_ <- UI.div #+ [UI.string "Dump mappe"]

        content <- UI.div #+ [UI.string (unDump dump)]

        picker <- UI.div #+
            [ mkFolderPicker "dumpPicker" "Vælg config folder" $ \folder ->
                when (folder /= "") $
                    withMVar files $ \ Files{..} ->
                        writeFile dumpFile (show folder)
            ]

        UI.div #+ fmap element [ title_, content, picker]


dumpSection :: Env -> Window -> Tabs -> Event (Either String Dump) -> UI ()
dumpSection env@Env{..} win tabs eDump = do
    dump <- liftIO $ withMVar files $ \ Files{..} -> getDump dumpFile
    bDump <- stepper dump eDump

    content <- UI.div # sink item (dumpView env <$> bDump)

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    view <- UI.div #+ fmap element
        [ tabs'
        , content
        , navigation
        ]

    void $ UI.getBody win # set children [view]
