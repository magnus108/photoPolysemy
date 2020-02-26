module Lib.Client.Dump
    ( dumpSection
    ) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Tab
import Lib.Dump
import Lib.Client.Tab
import Lib.Client.Element

import Lib.App (Env(..), Files(..))
import Control.Concurrent.MVar (withMVar)


dumpView :: Env -> Dump -> UI Element
dumpView Env{..} (Dump dump) = do
    title_ <- UI.div #+ [UI.string "Dump mappe"]
    content <- UI.div #+ [UI.string dump]

    picker <- UI.div #+
        [ mkFolderPicker "dumpPicker" "Vælg config folder" $ \folder ->
            when (folder /= "") $
                withMVar files $ \ Files{..} ->
                    writeFile dumpFile (show folder)
        ]

    UI.div #+ fmap element [ title_, content, picker]


dumpSection :: Env -> Dump -> Tabs -> UI Element
dumpSection env@Env{..} dump tabs = do

    content <- dumpView env dump

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    UI.div #+ fmap element
        [ tabs'
        , content
        , navigation
        ]
