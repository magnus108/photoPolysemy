module Lib.Client.Camera
    ( camerasSection
    ) where


import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Translation
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


camerasSection :: Env -> Window -> Translation -> Event (Either String Cameras) -> Tabs -> UI ()
camerasSection env@Env{..} win translation eCameras tabs = do
    cameras <- liftIO $ withMVar files $ \ Files{..} -> getCameras camerasFile

    bCameras <- stepper cameras eCameras
    content <- UI.div # sink item (mkCameras env <$> bCameras)

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env translation tabs

    view <- UI.div #+ fmap element
        [ tabs'
        , content
        , navigation
        ]

    void $ UI.getBody win # set children [view]


mkCameras :: Env -> Either String Cameras -> UI Element
mkCameras env = \case
    Left _ -> UI.div # set text "Camera ikke valgt" --TODO det her skulle jo aldrig ske

    Right (Cameras cameras) -> do
        let currentCamera = focus cameras
        let elems = cameras =>> \cameras'' -> let
                    thisCamera = focus cameras''
                in
                    ( thisCamera
                    , thisCamera == currentCamera
                    , Cameras cameras''
                    )
        cameras' <- mapM (mkCamera env) elems
        UI.div #. "buttons has-addons" # set children (ListZipper.toList cameras')

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
