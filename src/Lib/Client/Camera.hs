module Lib.Client.Camera
    ( camerasSection
    ) where


import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Tab
import Lib.Camera
import Lib.Client.Tab

import Lib.App (Env(..), Files(..))

import Lib.Client.Element
import Utils.Comonad
import Utils.ListZipper (focus)
import qualified Utils.ListZipper as ListZipper

import Control.Concurrent.MVar


camerasSection :: Env -> Cameras -> Tabs -> UI Element
camerasSection env@Env{..} cameras tabs = do

    content <- mkCameras env cameras

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    UI.div #+ fmap element
        [ tabs'
        , content
        , navigation
        ]


mkCameras :: Env -> Cameras -> UI Element
mkCameras env (Cameras cameras) = do
    cameras' <- mapM (mkCamera env) elems
    UI.div #. "buttons has-addons" #+ fmap element (ListZipper.toList cameras')
        where 
            --TODO Lav en indexedmap for zippers
            currentCamera = focus cameras
            elems = cameras =>> \cameras'' ->
                let
                    thisCamera = focus cameras''
                in
                    ( thisCamera
                    , thisCamera == currentCamera
                    , Cameras cameras'')


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
