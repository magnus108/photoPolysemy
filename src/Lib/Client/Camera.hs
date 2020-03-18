module Lib.Client.Camera
    ( camerasSection
    ) where


import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Tab
import Lib.Camera
import Lib.Client.Tab

import Lib.App (Env(..), Files(..))

import Lib.Client.Utils
import Lib.Client.Element
import Utils.Comonad
import Utils.ListZipper (focus)
import qualified Utils.ListZipper as ListZipper

import Control.Concurrent.MVar


camerasSection :: Env -> Window -> Behavior Cameras -> Tabs -> UI ()
camerasSection env@Env{..} win bCameras tabs = do

    content <- mkCameras env bCameras

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    view <- UI.div #+ fmap element
        [ tabs'
        , content
        , navigation
        ]

    void $ UI.getBody win # set children [view]


mkCameras :: Env -> Behavior Cameras -> UI Element
mkCameras env bCameras = do
    let bCameras' = bCameras <&> \(Cameras cameras) -> do
            let currentCamera = focus cameras
            let elems = cameras =>> \cameras'' -> let
                        thisCamera = focus cameras''
                    in
                        ( thisCamera
                        , thisCamera == currentCamera
                        , Cameras cameras''
                        )
            let cameras' = fmap (mkCamera env) elems
            ListZipper.toList cameras'

    UI.div #. "buttons has-addons" # sink items bCameras'



mkCamera :: Env -> (Camera, Bool, Cameras) -> UI Element
mkCamera Env{..} (camera, isCenter, cameras)
    | isCenter = do
        let name = show camera
        mkButton "idd" name #. "button is-selected" # set (attr "disabled") "true"
    | otherwise = do
        let name = show camera
        button <- mkButton "idd" name
        UI.on UI.click button $ \_ ->
            liftIO $ withMVar files $ \ Files{..} ->
                writeCameras camerasFile cameras
        return button
