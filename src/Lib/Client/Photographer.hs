module Lib.Client.Photographer
    ( photographersSection
    , Data(..)
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
import Lib.Photographer

import Lib.Client.Utils
import Lib.Client.Element



photographersSection :: Env -> Window -> Translation -> Tabs -> Behavior Model -> UI ()
photographersSection env@Env{..} win translation tabs bModel = do
    let bView = mkPhotographers env translation <$> bModel
    content <- UI.div #. "section" # sink item bView

    tabs' <- mkElement "nav" #. "section" #+ [mkTabs env tabs]
    navigation <- mkElement "footer" #. "section" #+ [mkNavigation env translation tabs]

    view <- UI.div #+ fmap element [ content ]

    void $ UI.getBody win # set children [tabs', view, navigation]


mkPhotographers :: Env -> Translation -> Model -> UI Element
mkPhotographers env@Env{..} translations model =
    case unModel model of
        NotAsked -> UI.p #+ [Lens.views starting string translations]
        Loading -> UI.p #+ [Lens.views loading string translations]
        Failure _ -> do
            err <- UI.p #+ [Lens.views photographersError string translations]
            picker <- mkFilePicker "photographerPicker" (Lens.view filePicker translations) $ \file ->
                when (file /= "") $ do
                    --TODO er det engentligt det her man vil?
                    parsePhotographers <- liftIO $ getPhotographers' file
                    forM_ parsePhotographers $ writePhotographers mPhotographersFile

            UI.div # set children [err, picker]

        Data (Photographers photographers) -> do
                let currentPhotographer = extract photographers
                let elems = photographers =>> \photographers''-> let
                                thisPhotographer = extract photographers''
                            in
                                ( thisPhotographer
                                , thisPhotographer == currentPhotographer
                                , Photographers photographers''
                                )
                elems' <- forM elems $ mkPhotographer env
                UI.div #. "buttons has-addons" # set children (toList elems')


mkPhotographer :: Env -> (Photographer, Bool, Photographers) -> UI Element
mkPhotographer Env{..} (photographer, isCenter, photographers)
    | isCenter = do
        let name = _name photographer
        mkButton name name #. "button is-selected" # set (attr "disabled") "true"
    | otherwise = do
        let name = _name photographer
        button <- mkButton name name
        UI.on UI.click button $ \_ ->
            writePhotographers mPhotographersFile photographers
        return button
