module Lib.Client.Camera
    ( camerasSection
    ) where


import           Reactive.Threepenny
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Utils.Comonad

import qualified Control.Lens as Lens

import Lib.App
import Lib.Translation
import Lib.Data
import Lib.Tab
import Lib.Client.Tab
import Lib.Camera

import Lib.Client.Utils
import Lib.Client.Element


camerasSection :: Env -> Window -> Translation -> Tabs -> Behavior Model -> UI ()
camerasSection env@Env{..} win translations tabs bModel = do

    content <- UI.div #. "section"

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
                    err <- UI.p #+ [Lens.views camerasError string translations]
                    picker <- mkFilePicker "cameraPicker" (Lens.view filePicker translations) $ \file ->
                        when (file /= "") $ do
                            --TODO er det engentligt det her man vil?
                            parseCameras <- liftIO $ getCameras' file
                            forM_ parseCameras $ writeCameras mCamerasFile

                    section <- UI.div # set children [err, picker]

                    _ <- element content # set children [section]
                    return ()

                Data (Cameras cameras) -> do
                        let currentCamera = extract cameras
                        let elems = cameras =>> \cameras'' -> let
                                        thisCamera = extract cameras''
                                    in
                                        ( thisCamera
                                        , thisCamera == currentCamera
                                        , Cameras cameras''
                                        )
                        elems' <- forM elems $ mkCamera env translations
                        
                        section <- UI.div #. "buttons has-addons" # set children (toList elems')
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
                err <- UI.p #+ [Lens.views camerasError string translations]
                picker <- mkFilePicker "cameraPicker" (Lens.view filePicker translations) $ \file ->
                    when (file /= "") $ do
                        --TODO er det engentligt det her man vil?
                        parseCameras <- liftIO $ getCameras' file
                        forM_ parseCameras $ writeCameras mCamerasFile

                section <- UI.div # set children [err, picker]
                _ <- element content # set children [section]
                return ()

            Data (Cameras cameras) -> do
                    let currentCamera = extract cameras
                    let elems = cameras =>> \cameras'' -> let
                                    thisCamera = extract cameras''
                                in
                                    ( thisCamera
                                    , thisCamera == currentCamera
                                    , Cameras cameras''
                                    )
                    elems' <- forM elems $ mkCamera env translations
                    
                    section <- UI.div #. "buttons has-addons" # set children (toList elems')
                    _ <- element content # set children [section]
                    return ()



    tabs' <- mkElement "nav" #. "section" #+ [mkTabs env translations tabs]
    navigation <- mkElement "footer" #. "section" #+ [mkNavigation env translations tabs]

    view <- UI.div #+ fmap element [ content ]

    void $ UI.getBody win # set children [tabs', view, navigation]


mkCamera :: Env -> Translation -> (Camera, Bool, Cameras) -> UI Element
mkCamera Env{..} translations (camera, isCenter, cameras)
    | isCenter = do
        mkButton "idd" name #. "button is-selected" # set (attr "disabled") "true"
    | otherwise = do
        button <- mkButton "idd" name
        UI.on UI.click button $ \_ ->
                writeCameras mCamerasFile cameras
        return button
    where
        translator = case camera of
                CR2 -> cr2
                CR3 -> cr3
        name = Lens.view translator translations
