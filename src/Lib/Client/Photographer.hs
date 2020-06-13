module Lib.Client.Photographer
    ( photographersSection
    , Data(..)
    ) where


import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Utils.Comonad

import qualified Control.Lens as Lens
import           Reactive.Threepenny

import Lib.App
import Lib.Translation
import Lib.Data
import Lib.Tab
import Lib.Client.Tab
import Lib.Photographer

import Lib.Client.Utils
import Lib.Client.Element



photographersSection :: Env -> Window -> Translation -> Tabs -> Behavior Model -> UI ()
photographersSection env@Env{..} win translations tabs bModel = do

    content <- UI.div

    liftIOLater $ do
        model <- currentValue bModel
        runUI win $ void $ do
            case unModel model of
                NotAsked -> do
                    msg <- Lens.views starting string translations
                    _ <- element content # set children [msg]
                    return ()
                Loading -> do
                    msg <- Lens.views loading string translations
                    _ <- element content # set children [msg]
                    return ()
                Failure e -> do
                    msg <- UI.p #+ [Lens.views photographersError string translations]
                    picker <- mkFilePicker "photographerPicker" (Lens.view filePicker translations) $ \file ->
                        when (file /= "") $ do
                            --TODO er det engentligt det her man vil?
                            parsePhotographers <- liftIO $ getPhotographers' file
                            forM_ parsePhotographers $ writePhotographers mPhotographersFile

                    section <- UI.div #. "section" # set children [msg, picker]
                    _ <- element content # set children [section]
                    return ()

                Data (Photographers photographers) -> do
                    let currentPhotographer = extract photographers
                    picker <- mkFilePicker "photographerPicker" (Lens.view filePicker translations) $ \file ->
                        when (file /= "") $ do
                            --TODO er det engentligt det her man vil?

                            
                            --BØR skrive fejl her
                            parsePhotographers <- liftIO $ getPhotographers' file
                            forM_ parsePhotographers $ writePhotographers mPhotographersFile

                    let elems = photographers =>> \photographers''-> let
                                    thisPhotographer = extract photographers''
                                in
                                    ( thisPhotographer
                                    , thisPhotographer == currentPhotographer
                                    , Photographers photographers''
                                    )
                    elems' <- forM elems $ mkPhotographer env
                    section <- UI.div #. "section" #+ [UI.div #. "buttons has-addons" # set children (toList elems'), element picker]
                    _ <- element content # set children [section]
                    return ()

    liftIOLater $ onChange bModel $ \model -> runUI win $ do
        case unModel model of
            NotAsked -> do
                msg <- Lens.views starting string translations
                _ <- element content # set children [msg]
                return ()
            Loading -> do
                msg <- Lens.views loading string translations
                _ <- element content # set children [msg]
                return ()
            Failure e -> do
                msg <- UI.p #+ [Lens.views photographersError string translations]
                picker <- mkFilePicker "photographerPicker" (Lens.view filePicker translations) $ \file ->
                    when (file /= "") $ do
                        --TODO er det engentligt det her man vil?
                        parsePhotographers <- liftIO $ getPhotographers' file
                        forM_ parsePhotographers $ writePhotographers mPhotographersFile

                section <- UI.div #. "section" # set children [msg, picker]
                _ <- element content # set children [section]
                return ()
            Data (Photographers photographers) -> do
                let currentPhotographer = extract photographers
                picker <- mkFilePicker "photographerPicker" (Lens.view filePicker translations) $ \file ->
                    when (file /= "") $ do
                        --TODO er det engentligt det her man vil?

                        
                        --BØR skrive fejl her
                        parsePhotographers <- liftIO $ getPhotographers' file
                        forM_ parsePhotographers $ writePhotographers mPhotographersFile

                let elems = photographers =>> \photographers''-> let
                                thisPhotographer = extract photographers''
                            in
                                ( thisPhotographer
                                , thisPhotographer == currentPhotographer
                                , Photographers photographers''
                                )
                elems' <- forM elems $ mkPhotographer env
                UI.div #+ [UI.div #. "buttons has-addons" # set children (toList elems'), element picker]
                section <- UI.div #. "section" #+ [UI.div #. "buttons has-addons" # set children (toList elems'), element picker]
                _ <- element content # set children [section]
                return ()


    tabs' <- mkElement "nav" #. "section" #+ [mkTabs env translations tabs]
    navigation <- mkElement "footer" #. "section" #+ [mkNavigation env translations tabs]

    view <- UI.div #+ fmap element [ content ]

    void $ UI.getBody win # set children [tabs', view, navigation]


mkPhotographer :: Env -> (Photographer, Bool, Photographers) -> UI Element
mkPhotographer Env{..} (photographer, isCenter, photographers)
    | isCenter = do
        let name' = _name photographer
        mkButton name' name' #. "button is-selected" # set (attr "disabled") "true"
    | otherwise = do
        let name' = _name photographer
        button <- mkButton name' name'
        UI.on UI.click button $ \_ ->
            writePhotographers mPhotographersFile photographers
        return button
