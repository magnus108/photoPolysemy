module Lib.Client.DagsdatoBackup
    ( dagsdatoBackupSection
    ) where


import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Tab
import Lib.DagsdatoBackup
import Lib.Client.Tab
import Lib.Client.Element

import Lib.Client.Utils

import Lib.App (Env(..), Files(..))
import Control.Concurrent.MVar (withMVar)


dagsdatoBackupView :: Env -> Behavior DagsdatoBackup -> UI Element
dagsdatoBackupView Env{..} bDagsdatoBackup = do
    let title_ = UI.div #+ [UI.string "DagsdatoBackup mappe"]
    let content = bDagsdatoBackup <&> \(DagsdatoBackup dagsdatoBackup) -> UI.div #+ [UI.string dagsdatoBackup]

    let picker = UI.div #+
            [ mkFolderPicker "dagsdatoBackupPicker" "Vælg config folder" $ \folder ->
                when (folder /= "") $
                    withMVar files $ \ Files{..} ->
                        writeFile dagsdatoBackupFile (show folder)
            ]

    UI.div # sink items (sequenceA [pure title_, content, pure picker])


dagsdatoBackupSection :: Env -> Window -> Behavior DagsdatoBackup -> Tabs -> UI ()
dagsdatoBackupSection env@Env{..} win bDagsdatoBackup tabs = do

    content <- dagsdatoBackupView env bDagsdatoBackup

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    view <- UI.div #+ fmap element
        [ tabs'
        , content
        , navigation
        ]

    void $ UI.getBody win #+ fmap element [view]
