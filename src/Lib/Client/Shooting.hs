module Lib.Client.Shooting
    ( shootingsSection
    ) where


import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Tab
import Lib.Shooting
import Lib.Client.Tab

import Lib.App (Env(..), Files(..))

import Lib.Client.Element
import Utils.Comonad
import Utils.ListZipper (focus)
import qualified Utils.ListZipper as ListZipper

import Control.Concurrent.MVar


shootingsSection :: Env -> Shootings -> Tabs -> UI Element
shootingsSection env@Env{..} shootings tabs = do

    content <- mkShootings env shootings

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    UI.div #+ fmap element
        [ tabs'
        , content
        , navigation
        ]


mkShootings :: Env -> Shootings -> UI Element
mkShootings env (Shootings shootings) = do
    shootings' <- mapM (mkShooting env) elems
    UI.div #. "buttons has-addons" #+ fmap element (ListZipper.toList shootings')
        where 
            --TODO Lav en indexedmap for zippers
            currentShooting = focus shootings
            elems = shootings =>> \shootings'' ->
                let
                    thisShooting = focus shootings''
                in
                    ( thisShooting
                    , thisShooting == currentShooting
                    , Shootings shootings'')


mkShooting :: Env -> (Shooting, Bool, Shootings) -> UI Element
mkShooting Env{..} (shooting, isCenter, shootings)
    | isCenter = do
        let name = show shooting
        mkButton "idd" name #. "button is-selected" # set (attr "disabled") "true"
    | otherwise = do
        let name = show shooting
        forwardButton <- mkButton "idd" name
        UI.on UI.click forwardButton $ \_ ->
            liftIO $ withMVar files $ \ Files{..} ->
                writeShootings camerasFile shootings
        return forwardButton
