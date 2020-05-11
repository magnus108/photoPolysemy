module Lib.Client.Dump
    ( dumpSection
    ) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import qualified Control.Lens as Lens

import Lib.Translation
import Lib.Tab
import Lib.Dump
import Lib.Client.Tab
import Lib.Client.Element
import Lib.Client.Utils

import Lib.Data

import Lib.App (Env(..), Files(..))
import Control.Concurrent.MVar (withMVar)


mkDump :: Env -> Translation -> DumpModel -> [UI Element]
mkDump env@Env{..} translations model = do
    case unModel model of
        NotAsked -> [UI.p #+ [Lens.views starting string translations]]
        Loading -> [UI.p #+ [Lens.views loading string translations]]
        Failure _ ->
            [ UI.div #. "section" #+ [Lens.views dumpError string translations]
                   , UI.div #. "section" #+
                       [ mkFolderPicker "dumpPicker" (Lens.view folderPicker translations) $ \folder ->
                            when (folder /= "") $
                                -- todo: handle bad input
                                void $ writeDump mDumpFile (Dump folder)
                        ]
                    ]

        Data (Dump dump) ->
            [ UI.div #. "section" #+ [UI.h2 #+ [Lens.views dumpTitle string translations], UI.string dump]
                   , UI.div #. "section" #+
                        [mkFolderPicker "dumpPicker" (Lens.view folderPicker translations) $ \folder ->
                            when (folder /= "") $
                                -- todo: handle bad input
                                void $ writeDump mDumpFile (Dump folder)
                        ]
                   ]


dumpSection :: Env -> Window -> Translation -> Tabs -> Behavior DumpModel -> UI ()
dumpSection env@Env{..} win translation tabs bModel = do
    let bView = mkDump env translation <$> bModel
    content <- UI.div # sink items bView

    tabs' <- mkElement "nav" #. "section" #+ [mkTabs env tabs]
    navigation <- mkElement "footer" #. "section" #+ [mkNavigation env translation tabs]

    view <- UI.div #+ fmap element [ content ]

    void $ UI.getBody win # set children [tabs', view, navigation]
