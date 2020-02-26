module Lib.Client.Doneshooting
    ( doneshootingSection
    ) where


import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Tab
import Lib.Doneshooting
import Lib.Client.Tab
import Lib.Client.Element


import Lib.App (Env(..), Files(..))
import Control.Concurrent.MVar (withMVar)

doneshootingView :: Env -> Doneshooting -> UI Element
doneshootingView Env{..} (Doneshooting doneshooting) = do
    title_ <- UI.string "Dump mappe"
    content <- UI.string doneshooting

    picker <- mkFolderPicker "doneshootingPicker" "Vælg config folder" $ \folder -> do
        when (folder /= "") $
            withMVar files $ \ Files{..} ->
                writeFile doneshootingFile (show folder)

    UI.div #+ fmap element [ title_, content, picker]


doneshootingSection :: Env -> Doneshooting -> Tabs -> UI Element
doneshootingSection env@Env{..} doneshooting tabs = do

    content <- doneshootingView env doneshooting

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    UI.div #+ fmap element
        [ tabs'
        , content
        , navigation
        ]
