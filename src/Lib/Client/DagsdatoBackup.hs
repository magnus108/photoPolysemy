module Lib.Client.DagsdatoBackup
    ( dagsdatoBackupSection
    ) where


import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Translation
import Lib.Tab
import Lib.DagsdatoBackup
import Lib.Client.Tab
import Lib.Client.Element

import Lib.Client.Utils

import Lib.App (Env(..), Files(..))
import Control.Concurrent.MVar (withMVar)


dagsdatoBackupView :: Env -> Either String DagsdatoBackup -> UI Element
dagsdatoBackupView Env{..} = \case
    Left _ -> do
        title_ <- UI.p # set text "Der er en fejl med dagsdato backup mappe"

        picker <- UI.div #+
                [ mkFolderPicker "dagsdatoBackupPicker" "Vælg config folder" $ \folder ->
                    when (folder /= "") $
                        withMVar files $ \ Files{..} ->
                            writeFile dagsdatoBackupFile (show folder)
                ]

        UI.div #+ fmap element [title_, picker]

    Right dagsdatoBackup -> do
        title_ <- UI.div #+ [UI.string "DagsdatoBackup mappe"]
        content <- UI.div #+ [UI.string (unDagsdatoBackup dagsdatoBackup)]

        picker <- UI.div #+
                [ mkFolderPicker "dagsdatoBackupPicker" "Vælg config folder" $ \folder ->
                    when (folder /= "") $
                        withMVar files $ \ Files{..} ->
                            writeFile dagsdatoBackupFile (show folder)
                ]

        UI.div #+ fmap element [title_, content, picker]


dagsdatoBackupSection :: Env -> Window -> Translation -> Event (Either String DagsdatoBackup) -> Tabs -> UI ()
dagsdatoBackupSection env@Env{..} win translation eDagsdatoBackup tabs = do

    dagsdatoBackup <- liftIO $ withMVar files $ \ Files{..} -> getDagsdatoBackup dagsdatoBackupFile
    bDagsdatoBackup <- stepper dagsdatoBackup eDagsdatoBackup

    content <- UI.div # sink item (dagsdatoBackupView env <$> bDagsdatoBackup)

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env translation tabs

    view <- UI.div #+ fmap element
        [ tabs'
        , content
        , navigation
        ]

    void $ UI.getBody win # set children [view]
