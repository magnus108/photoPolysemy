module Lib.Client.Doneshooting
    ( doneshootingSection
    ) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI


import qualified Control.Lens as Lens

import Lib.App
import Lib.Translation
import Lib.Data
import Lib.Tab
import Lib.Doneshooting
import Lib.Client.Tab
import Lib.Client.Utils
import Lib.Client.Element


mkDoneshooting :: Env -> Translation -> Model -> [UI Element]
mkDoneshooting Env{..} translations model = do
    case unModel model of
        NotAsked -> [UI.p #+ [Lens.views starting string translations]]
        Loading -> [UI.p #+ [Lens.views loading string translations]]
        Failure e ->
            [ UI.div #. "section" #+ [Lens.views doneshootingError string translations, UI.div #+ [string e]]
                   , UI.div #. "section" #+
                       [ mkFolderPicker "doneshootingPicker" (Lens.view folderPicker translations) $ \folder ->
                            when (folder /= "") $
                                -- todo: handle bad input
                                void $ writeDoneshooting mDoneshootingFile (Doneshooting folder)
                        ]
                    ]

        Data (Doneshooting doneshooting') ->
            [ UI.div #. "section" #+ [UI.h2 #+ [Lens.views doneshootingTitle string translations], UI.string doneshooting']
                   , UI.div #. "section" #+
                        [mkFolderPicker "doneshootingPicker" (Lens.view folderPicker translations) $ \folder ->
                            when (folder /= "") $
                                -- todo: handle bad input
                                void $ writeDoneshooting mDoneshootingFile (Doneshooting folder)
                        ]
                   ]


doneshootingSection :: Env -> Window -> Translation -> Tabs -> Behavior Model -> UI ()
doneshootingSection env@Env{..} win translation tabs bModel = do
    let bView = mkDoneshooting env translation <$> bModel
    content <- UI.div # sink items bView

    tabs' <- mkElement "nav" #. "section" #+ [mkTabs env translation tabs]
    navigation <- mkElement "footer" #. "section" #+ [mkNavigation env translation tabs]

    view <- UI.div #+ fmap element [ content ]

    void $ UI.getBody win # set children [tabs', view, navigation]
