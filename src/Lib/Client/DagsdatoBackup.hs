module Lib.Client.DagsdatoBackup
    ( dagsdatoBackupSection
    ) where


import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Tab
import Lib.DagsdatoBackup
import Lib.Client.Tab
import Lib.Client.Element

import Lib.App (Env(..), Files(..))
import Control.Concurrent.MVar (withMVar)


dagsdatoBackupView :: Env -> DagsdatoBackup -> UI Element
dagsdatoBackupView Env{..} (DagsdatoBackup dagsdatoBackup) = do
    title_ <- UI.div #+ [UI.string "DagsdatoBackup mappe"]
    content <- UI.div #+ [UI.string dagsdatoBackup]

    picker <- UI.div #+
        [ mkFolderPicker "dagsdatoBackupPicker" "Vælg config folder" $ \folder ->
            when (folder /= "") $
                withMVar files $ \ Files{..} ->
                    writeFile dagsdatoBackupFile (show folder)
        ]

    UI.div #+ fmap element [ title_, content, picker]


dagsdatoBackupSection :: Env -> DagsdatoBackup -> Tabs -> UI Element
dagsdatoBackupSection env@Env{..} dagsdatoBackup tabs = do

    content <- dagsdatoBackupView env dagsdatoBackup

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    UI.div #+ fmap element
        [ tabs'
        , content
        , navigation
        ]
