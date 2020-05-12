module Lib.Client.Camera
    ( camerasSection
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
import Lib.Camera

import Lib.Client.Utils
import Lib.Client.Element


camerasSection :: Env -> Window -> Translation -> Tabs -> Behavior Model -> UI ()
camerasSection env@Env{..} win translation tabs bModel = do
    let bView = mkCameras env translation <$> bModel
    content <- UI.div #. "section" # sink item bView

    tabs' <- mkElement "nav" #. "section" #+ [mkTabs env tabs]
    navigation <- mkElement "footer" #. "section" #+ [mkNavigation env translation tabs]

    view <- UI.div #+ fmap element [ content ]

    void $ UI.getBody win # set children [tabs', view, navigation]


mkCameras :: Env -> Translation -> Model -> UI Element
mkCameras env@Env{..} translations model =
    case unModel model of
        NotAsked -> UI.p #+ [Lens.views starting string translations]
        Loading -> UI.p #+ [Lens.views loading string translations]
        Failure _ -> do
            err <- UI.p #+ [Lens.views camerasError string translations]
            picker <- mkFilePicker "cameraPicker" (Lens.view filePicker translations) $ \file ->
                when (file /= "") $ do
                    --TODO er det engentligt det her man vil?
                    parseCameras <- liftIO $ getCameras' file
                    forM_ parseCameras $ writeCameras mCamerasFile

            UI.div # set children [err, picker]

        Data (Cameras cameras) -> do
                let currentCamera = extract cameras
                let elems = cameras =>> \cameras'' -> let
                                thisCamera = extract cameras''
                            in
                                ( thisCamera
                                , thisCamera == currentCamera
                                , Cameras cameras''
                                )
                elems' <- forM elems $ mkCamera env
                UI.div #. "buttons has-addons" # set children (toList elems')

mkCamera :: Env -> (Camera, Bool, Cameras) -> UI Element
mkCamera Env{..} (camera, isCenter, cameras)
    | isCenter = do
        let name = show camera
        mkButton "idd" name #. "button is-selected" # set (attr "disabled") "true"
    | otherwise = do
        let name = show camera
        button <- mkButton "idd" name
        UI.on UI.click button $ \_ ->
                writeCameras mCamerasFile cameras
        return button
