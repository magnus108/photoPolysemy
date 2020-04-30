module Lib.Client.Photographer
    ( photographersSection
    , Data(..)
    ) where


import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Data
import Lib.Tab
import Lib.Photographer
import Lib.Client.Tab

import Lib.App (Env(..))

import Lib.Client.Utils
import Lib.Client.Element
import Utils.Comonad
import Utils.ListZipper (focus)
import qualified Utils.ListZipper as ListZipper


photographersSection :: Env -> Window -> Tabs -> Behavior Model -> UI ()
photographersSection env@Env{..} win tabs bModel = do
    let bView = mkPhotographers env <$> bModel
    content <- UI.div # sink item bView

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    view <- UI.div #+ fmap element
        [ tabs'
        , content
        , navigation
        ]

    void $ UI.getBody win # set children [view]

    --MANGLER TRANSLATIONS
    --MANGERL OPTRYDNING I imports
    --MANGLER OPTRYDNING i tabs og navigation
    --Mangler OPTRYDNING i mkPhotographers


mkPhotographers :: Env -> Model -> UI Element
mkPhotographers env@Env{..} model =
    case unModel model of
        NotAsked -> UI.div #+ [string "Starting.."]
        Loading -> UI.div #+ [string "Loading.."]
        Data (Photographers photographers) -> do
                let currentPhotographer = focus photographers
                let elems = photographers =>> \photographers''-> let
                                thisPhotographer = focus photographers''
                            in
                                ( thisPhotographer
                                , thisPhotographer == currentPhotographer
                                , Photographers photographers''
                                )
                elems' <- mapM (mkPhotographer env) elems
                UI.div #. "buttons has-addons" # set children (ListZipper.toList elems')
        Failure _ -> do
            picker <- mkFilePicker "photographerPicker" "Vælg import fil" $ \file -> 
                when (file /= "") $ do
                    --TODO er det engentligt det her man vil?
                    --TODO og hvad gør vi med fejl?
                    --TODO med nuværende løsning er COPY nok
                    --TODO FEJL BLIVER ignoret med denne løsning
                    photographers <- liftIO $ getPhotographers' file
                    forM_ photographers $ writePhotographers mPhotographersFile

            para <- UI.p # set text "Der er en fejl med fotografer"
            UI.div # set children [para, picker]


mkPhotographer :: Env -> (Photographer, Bool, Photographers) -> UI Element
mkPhotographer Env{..} (photographer, isCenter, photographers)
    | isCenter = do
        let name = _name photographer
        mkButton "idd" name #. "button is-selected" # set (attr "disabled") "true"
    | otherwise = do
        let name = _name photographer
        forwardButton <- mkButton "idd" name
        UI.on UI.click forwardButton $ \_ -> writePhotographers mPhotographersFile photographers
        return forwardButton
