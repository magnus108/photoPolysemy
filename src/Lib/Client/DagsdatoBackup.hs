module Lib.Client.DagsdatoBackup
    ( dagsdatoBackupSection
    ) where


import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI


import qualified Control.Lens as Lens

import Lib.App
import Lib.Translation
import Lib.Data
import Lib.Tab
import Lib.DagsdatoBackup
import Lib.Client.Tab
import Lib.Client.Utils
import Lib.Client.Element


mkDagsdatoBackup :: Env -> Translation -> Model -> [UI Element]
mkDagsdatoBackup Env{..} translations model = do
    case unModel model of
        NotAsked -> [UI.p #+ [Lens.views starting string translations]]
        Loading -> [UI.p #+ [Lens.views loading string translations]]
        Failure _ ->
            [ UI.div #. "section" #+ [Lens.views dagsdatoBackupError string translations]
                   , UI.div #. "section" #+
                       [ mkFolderPicker "dagsdatoBackupPicker" (Lens.view folderPicker translations) $ \folder ->
                            when (folder /= "") $
                                -- todo: handle bad input
                                void $ writeDagsdatoBackup mDagsdatoBackupFile (DagsdatoBackup folder)
                        ]
                    ]

        Data (DagsdatoBackup dagsdatoBackup') ->
            [ UI.div #. "section" #+ [UI.h2 #+ [Lens.views dagsdatoTitle string translations], UI.string dagsdatoBackup']
                   , UI.div #. "section" #+
                        [mkFolderPicker "dagsdatoPicker" (Lens.view folderPicker translations) $ \folder ->
                            when (folder /= "") $
                                -- todo: handle bad input
                                void $ writeDagsdatoBackup mDagsdatoBackupFile (DagsdatoBackup folder)
                        ]
                   ]


dagsdatoBackupSection :: Env -> Window -> Translation -> Tabs -> Behavior Model -> UI ()
dagsdatoBackupSection env@Env{..} win translation tabs bModel = do
    let bView = mkDagsdatoBackup env translation <$> bModel
    content <- UI.div # sink items bView

    tabs' <- mkElement "nav" #. "section" #+ [mkTabs env translation tabs]
    navigation <- mkElement "footer" #. "section" #+ [mkNavigation env translation tabs]

    view <- UI.div #+ fmap element [ content ]

    void $ UI.getBody win # set children [tabs', view, navigation]
