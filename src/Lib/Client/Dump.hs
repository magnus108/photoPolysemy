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


dumpView :: Env -> Behavior Dump -> UI Element
dumpView Env{..} bDump = do

    title_ <- UI.div #+ [UI.string "Dump mappe"]

    let content' = bDump <&> \(Dump dump) -> [UI.string (show dump)]

    content <- UI.div # sink items content'

    picker <- UI.div #+
        [ mkFolderPicker "dumpPicker" "Vælg config folder" $ \folder ->
            when (folder /= "") $
                withMVar files $ \ Files{..} ->
                    writeFile dumpFile (show folder)
        ]

    UI.div #+ fmap element [ title_, content, picker]


dumpSection :: Env -> Window -> Behavior Dump -> Tabs -> UI ()
dumpSection env@Env{..} win bDump tabs = do

    content <- dumpView env bDump

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    view <- UI.div #+ fmap element
        [ tabs'
        , content
        , navigation
        ]

    void $ UI.getBody win # set children [view]
