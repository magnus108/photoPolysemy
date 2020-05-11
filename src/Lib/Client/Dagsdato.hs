module Lib.Client.Dagsdato
    ( dagsdatoSection
    ) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI


import qualified Control.Lens as Lens

import Lib.App
import Lib.Translation
import Lib.Data
import Lib.Tab
import Lib.Dagsdato
import Lib.Client.Tab
import Lib.Client.Utils
import Lib.Client.Element


mkDagsdato :: Env -> Translation -> Model -> [UI Element]
mkDagsdato Env{..} translations model = do
    case unModel model of
        NotAsked -> [UI.p #+ [Lens.views starting string translations]]
        Loading -> [UI.p #+ [Lens.views loading string translations]]
        Failure _ ->
            [ UI.div #. "section" #+ [Lens.views dagsdatoError string translations]
                   , UI.div #. "section" #+
                       [ mkFolderPicker "dagsdatoPicker" (Lens.view folderPicker translations) $ \folder ->
                            when (folder /= "") $
                                -- todo: handle bad input
                                void $ writeDagsdato mDagsdatoFile (Dagsdato folder)
                        ]
                    ]

        Data (Dagsdato dagsdato') ->
            [ UI.div #. "section" #+ [UI.h2 #+ [Lens.views dagsdatoTitle string translations], UI.string dagsdato']
                   , UI.div #. "section" #+
                        [mkFolderPicker "dagsdatoPicker" (Lens.view folderPicker translations) $ \folder ->
                            when (folder /= "") $
                                -- todo: handle bad input
                                void $ writeDagsdato mDagsdatoFile (Dagsdato folder)
                        ]
                   ]


dagsdatoSection :: Env -> Window -> Translation -> Tabs -> Behavior Model -> UI ()
dagsdatoSection env@Env{..} win translation tabs bModel = do
    let bView = mkDagsdato env translation <$> bModel
    content <- UI.div # sink items bView

    tabs' <- mkElement "nav" #. "section" #+ [mkTabs env tabs]
    navigation <- mkElement "footer" #. "section" #+ [mkNavigation env translation tabs]

    view <- UI.div #+ fmap element [ content ]

    void $ UI.getBody win # set children [tabs', view, navigation]
