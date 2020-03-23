module Lib.Client.Photographer
    ( photographersSection
    ) where


import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Tab
import Lib.Photographer
import Lib.Client.Tab

import Lib.App (Env(..), Files(..))

import Lib.Client.Utils
import Lib.Client.Element
import Utils.Comonad
import Utils.ListZipper (focus)
import qualified Utils.ListZipper as ListZipper

import Control.Concurrent.MVar


photographersSection :: Env -> Window -> Event Photographers -> Tabs -> UI ()
photographersSection env@Env{..} win ePhotographers tabs = do
    photographers <- liftIO $ withMVar files $ \ Files{..} -> getPhotographers photographersFile

    bPhotographers <- mkPhotographers env <<$>> stepper photographers ePhotographers

    content <- UI.div
            #. "buttons has-addons"
            # sink items bPhotographers

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    view <- UI.div #+ fmap element
        [ tabs'
        , content
        , navigation
        ]

    void $ UI.getBody win # set children [view]


mkPhotographers :: Env -> Photographers -> [UI Element]
mkPhotographers env (Photographers photographers) = ListZipper.toList photographers'
    where
        currentPhotographer = focus photographers
        elems = photographers =>> \photographers''-> let
                    thisPhotographer = focus photographers''
                in
                    ( thisPhotographer
                    , thisPhotographer == currentPhotographer
                    , Photographers photographers''
                    )
        photographers' = mkPhotographer env <$> elems




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
