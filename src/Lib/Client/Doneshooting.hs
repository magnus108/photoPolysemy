module Lib.Client.Doneshooting
    ( doneshootingSection
    ) where


import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Tab
import Lib.Doneshooting
import Lib.Client.Tab
import Lib.Client.Element

import Lib.Client.Utils

import Lib.App (Env(..), Files(..))
import Control.Concurrent.MVar (withMVar)

doneshootingView :: Env -> Behavior Doneshooting -> UI Element
doneshootingView Env{..} bDoneshooting = do
    let title_ = UI.div #+ [UI.string "Doneshooting mappe"]

    let content = bDoneshooting <&> \(Doneshooting doneshooting) -> UI.div #+ [UI.string doneshooting]

    let picker = UI.div #+
            [ mkFolderPicker "doneshootingPicker" "Vælg config folder" $ \folder ->
                when (folder /= "") $
                    withMVar files $ \ Files{..} ->
                        writeFile doneshootingFile (show folder)
            ]

    UI.div # sink items (sequenceA [pure title_, content, pure picker])


doneshootingSection :: Env -> Window -> Behavior Doneshooting -> Tabs -> UI ()
doneshootingSection env@Env{..} win bDoneshooting tabs = do

    content <- doneshootingView env bDoneshooting

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    view <- UI.div #+ fmap element
        [ tabs'
        , content
        , navigation
        ]

    void $ UI.getBody win #+ fmap element [view]
