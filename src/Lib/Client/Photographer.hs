module Lib.Client.Photographer
    ( photographersSection
    ) where


import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Tab
import Lib.Photographer
import Lib.Client.Tab

import Lib.App (Env(..), Files(..))

import Lib.Client.Element
import Utils.Comonad
import Utils.ListZipper (focus)
import qualified Utils.ListZipper as ListZipper

import Control.Concurrent.MVar


photographersSection :: Env -> Photographers -> Tabs -> UI Element
photographersSection env@Env{..} photographers tabs = do

    content <- mkPhotographers env photographers

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    UI.div #+ fmap element
        [ tabs'
        , content
        , navigation
        ]


mkPhotographers :: Env -> Photographers -> UI Element
mkPhotographers env (Photographers photographers) = do
    photographers' <- mapM (mkPhotographer env) elems
    UI.div #. "buttons has-addons" #+ fmap element (ListZipper.toList photographers')
        where 
            --TODO Lav en indexedmap for zippers
            currentPhotographer = focus photographers
            elems = photographers =>> \photographers'' ->
                let
                    thisPhotographer = focus photographers''
                in
                    (thisPhotographer
                    , thisPhotographer == currentPhotographer
                    , Photographers photographers'')


mkPhotographer :: Env -> (Photographer, Bool, Photographers) -> UI Element
mkPhotographer Env{..} (photographer, isCenter, photographers)
    | isCenter = do
        let name = _name photographer
        mkButton "idd" name #. "button is-selected" # set (attr "disabled") "true"
    | otherwise = do
        let name = _name photographer
        forwardButton <- mkButton "idd" name
        UI.on UI.click forwardButton $ \_ ->
            liftIO $ withMVar files $ \ Files{..} ->
                writePhotographers photographersFile photographers
        return forwardButton
