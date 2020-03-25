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


photographersSection :: Env -> Window -> Event (Either String Photographers) -> Tabs -> UI ()
photographersSection env@Env{..} win ePhotographers tabs = do
    photographers <- liftIO $ withMVar files $ \ Files{..} -> getPhotographers photographersFile

    bPhotographers <- stepper photographers ePhotographers

    content <- UI.div # sink item (mkPhotographers env <$> bPhotographers)

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    view <- UI.div #+ fmap element
        [ tabs'
        , content
        , navigation
        ]

    void $ UI.getBody win # set children [view]


mkPhotographers :: Env -> Either String Photographers -> UI Element
mkPhotographers env@Env{..} photographers' =  case photographers' of
    Left e -> do

        picker <- mkFilePicker "photographerPicker" "Vælg import fil" $ \file -> 
            when (file /= "") $ do
                --TODO er det engentligt det her man vil?
                --TODO og hvad gør vi med fejl?
                --TODO med nuværende løsning er COPY nok
                --TODO FEJL BLIVER ignoret med denne løsning
                photographers <- liftIO $ getPhotographers file
                liftIO $ withMVar files $ \ Files{..} -> do
                    mapM_ (writePhotographers photographersFile) photographers

        para <- UI.p # set text "Der er en fejl med fotografer"
        UI.div # set children [para, picker]

    Right (Photographers photographers) -> do
        let currentPhotographer = focus photographers
        let elems = photographers =>> \photographers''-> let
                        thisPhotographer = focus photographers''
                    in
                        ( thisPhotographer
                        , thisPhotographer == currentPhotographer
                        , Photographers photographers''
                        )
        elems' <- mapM (mkPhotographer env) elems
        UI.div #. "buttons has-addons"
               # set children (ListZipper.toList elems')

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
