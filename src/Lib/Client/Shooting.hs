module Lib.Client.Shooting
    ( shootingsSection
    ) where


import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Utils.Comonad

import qualified Control.Lens as Lens

import Lib.App
import Lib.Translation
import Lib.Data
import Lib.Tab
import Lib.Client.Tab
import Lib.Shooting

import Lib.Client.Utils
import Lib.Client.Element


shootingsSection :: Env -> Window -> Translation -> Tabs -> Behavior Model -> UI ()
shootingsSection env@Env{..} win translation tabs bModel = do
    let bView = mkShootings env translation <$> bModel
    content <- UI.div #. "section" # sink item bView

    tabs' <- mkElement "nav" #. "section" #+ [mkTabs env tabs]
    navigation <- mkElement "footer" #. "section" #+ [mkNavigation env translation tabs]

    view <- UI.div #+ fmap element [ content ]

    void $ UI.getBody win # set children [tabs', view, navigation]


mkShootings :: Env -> Translation -> Model -> UI Element
mkShootings env@Env{..} translations model =
    case unModel model of
        NotAsked -> UI.p #+ [Lens.views starting string translations]
        Loading -> UI.p #+ [Lens.views loading string translations]
        Failure _ -> do
            err <- UI.p #+ [Lens.views shootingsError string translations]
            picker <- mkFilePicker "shootingPicker" (Lens.view filePicker translations) $ \file ->
                when (file /= "") $ do
                    --TODO er det engentligt det her man vil?
                    parseShootings <- liftIO $ getShootings' file
                    forM_ parseShootings $ writeShootings mShootingsFile

            UI.div # set children [err, picker]

        Data (Shootings shootings) -> do
                let currentShooting = extract shootings
                let elems = shootings =>> \shootings'' -> let
                                thisShooting = extract shootings''
                            in
                                ( thisShooting
                                , thisShooting == currentShooting
                                , Shootings shootings''
                                )
                elems' <- forM elems $ mkShooting env translations
                UI.div #. "buttons has-addons" # set children (toList elems')


mkShooting :: Env -> Translation -> (Shooting, Bool, Shootings) -> UI Element
mkShooting Env{..} translations (shooting, isCenter, shootings)
    | isCenter = do
        mkButton "idd" name #. "button is-selected" # set (attr "disabled") "true"
    | otherwise = do
        button <- mkButton "idd" name
        UI.on UI.click button $ \_ ->
                writeShootings mShootingsFile shootings
        return button
    where
        translator = case shooting of
                ReShoot -> reShoot
                Normal -> normal
        name = Lens.view translator translations
