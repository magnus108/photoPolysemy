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

import Control.Concurrent



photographersSection :: Env -> Window -> Event (Data String Photographers) -> Tabs -> UI ()
photographersSection env@Env{..} win ePhotographers tabs = do
    let (_, eLoading, ePhotographersErr, ePhotographersSucc) = splitData ePhotographers

    (eInitial, eInitialHandle) <- liftIO newEvent

    _ <- getPhotographers mPhotographersFile eInitialHandle


    bModel <- stepper initalState $ head <$> unions'
        ((Model . Data <$> ePhotographersSucc)
            :| [ Model . Failure <$> ePhotographersErr
               , Model <$> eInitial
               , Model Loading <$ eLoading
               ])

    content <- UI.div # sink item (mkPhotographers env <$> bModel)

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    view <- UI.div #+ fmap element
        [ tabs'
        , content
        , navigation
        ]

    void $ UI.getBody win # set children [view]




mkPhotographers :: Env -> Model -> UI Element
mkPhotographers env@Env{..} model =
    case unModel model of
        NotAsked -> UI.div # set text "Starting.."
        Loading -> UI.div # set text "Loading.."
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
                    liftIO $ withMVar mPhotographersFile $ \photographersFile -> do
                        mapM_ (writePhotographers photographersFile) photographers

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
        UI.on UI.click forwardButton $ \_ ->
            liftIO $ withMVar mPhotographersFile $ \file ->
                writePhotographers file photographers
        return forwardButton
