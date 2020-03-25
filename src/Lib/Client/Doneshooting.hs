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

doneshootingView :: Env -> Either String Doneshooting -> UI Element
doneshootingView Env{..} = \case
    Left _ -> do
        title_ <- UI.p # set text "Doneshooting mappe ikke valgt"

        picker <- UI.div #+
                [ mkFolderPicker "doneshootingPicker" "Vælg config folder" $ \folder ->
                    when (folder /= "") $
                        withMVar files $ \ Files{..} ->
                            writeFile doneshootingFile (show folder)
                ]

        UI.div #+ fmap element [title_, picker]

    Right doneshooting -> do
        title_ <- UI.div #+ [UI.string "Doneshooting mappe"]

        content <- UI.div #+ [UI.string (unDoneshooting doneshooting)]

        picker <- UI.div #+
                [ mkFolderPicker "doneshootingPicker" "Vælg config folder" $ \folder ->
                    when (folder /= "") $
                        withMVar files $ \ Files{..} ->
                            writeFile doneshootingFile (show folder)
                ]

        UI.div #+ fmap element [title_, content, picker]


doneshootingSection :: Env -> Window -> Event (Either String Doneshooting) -> Tabs -> UI ()
doneshootingSection env@Env{..} win eDoneshooting tabs = do
    doneshooting <- liftIO $ withMVar files $ \ Files{..} -> getDoneshooting doneshootingFile
    bDoneshooting <- stepper doneshooting eDoneshooting

    content <- UI.div # sink item (doneshootingView env <$> bDoneshooting)

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    view <- UI.div #+ fmap element
        [ tabs'
        , content
        , navigation
        ]

    void $ UI.getBody win # set children [view]
