module Lib.Client.Main
    ( mainSection
    , mkModel
    ) where

import Utils.Comonad

import           Control.Monad
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Data
import Lib.Translation
import Lib.Tab
import qualified Lib.Photographee as Photographee
import qualified Lib.Grade as Grade
import qualified Lib.Dump as Dump
import qualified Lib.Location as Location
import Lib.Client.Tab
import qualified Lib.Client.Location as CLocation
import Lib.Client.Utils

import Lib.App (Env(..), Files(..))
import Control.Concurrent.MVar

import qualified Control.Lens as Lens

import           Reactive.Threepenny
import Lib.Client.Utils
import Lib.Client.Element


data Item = Item { location :: Location.LocationFile
                 , grades :: Grade.Grades
                 , dump :: Dump.Dump
                 , dumpDir :: Dump.DumpDir --TODO this is wrong
                 , photograhees :: Photographee.Photographees
                 }

newtype Model = Model { unModel :: Data String Item }

mkModel :: Location.Model -> Grade.Model -> Dump.DumpModel -> Dump.DumpDirModel -> Photographee.Model -> Model
mkModel location grades dump dumpDir photograhees =
    Model $ Item <$> Location.unModel location <*> Grade._grades grades <*> Dump.unModel dump <*> Dump.unDumpDirModel dumpDir <*> (Lens.view Photographee.unModel photograhees)


photograheesList :: Env -> Window -> Photographee.Photographees -> [UI Element]
photograheesList env win (Photographee.Photographees photographees') = do
        let currentPhotographee = extract photographees'
        let elems = photographees' =>> \photographees''-> let
                        thisPhotographee = extract photographees''
                    in
                        ( thisPhotographee
                        , thisPhotographee == currentPhotographee
                        , Photographee.Photographees photographees''
                        )
        (mkPhotographee env) <$> toList elems


mkPhotographee :: Env -> (Photographee.Photographee, Bool, Photographee.Photographees) -> UI Element
mkPhotographee Env{..} (photographee, isCenter, photographees)
    | isCenter = do
        let name = Lens.view Photographee.name photographee
        UI.div #. "section" #+ [mkButton name name #. "button is-selected" # set (attr "disabled") "true"]
    | otherwise = do
        let name = Lens.view Photographee.name photographee
        button <- mkButton name name
        UI.on UI.click button $ \_ ->
            return ()
        UI.div #. "section" #+ [element button]


gradeItem
    :: Env
    -> Window
    -> Translation
    -> Behavior Model
    -> UI ((Element, Element), Tidings Model)
gradeItem env win translations bModel = do
    let bItem   = toJust <$> unModel <$> bModel
        bGrades = grades <<$>> bItem
        bPhotographees = photograhees <<$>> bItem

    select         <- UI.select
    bEditingSelect <- bEditing select
    liftIOLater $ onChange bGrades $ \s -> runUI win $ do
        editing <- liftIO $ currentValue bEditingSelect
        when (not editing) $ void $ do
            let options = maybe [] (CLocation.mkGrades env) s
            element select # set children [] #+ options

    let eSelect   = CLocation.selectGrade <$> filterJust (selectionChange' select)

    let allEvents = concatenate' <$> unions' (eSelect :| [])


    photograhees' <- UI.div # sink items (maybe [] (photograheesList env win) <$> bPhotographees)

    let
        superTide =
            tidings bModel
                $   filterJust
                $   fmap
                        (\m f -> case toJust (unModel m) of
                            Nothing -> Nothing
                            Just x ->
                                Just
                                    (Model
                                        (Data (Item (location x) (f (grades x)) (dump x) (dumpDir x) (photograhees x)))
                                    )
                        )
                        bModel
                <@> allEvents


    return ((getElement select, getElement photograhees'), superTide)


mainSection :: Env -> Window -> Translation -> Tabs -> Behavior Model -> UI ()
mainSection env@Env{..} win translations tabs bModel = do
    view                              <- UI.div

    ((select, photographees'), tModel) <- gradeItem env win translations bModel
    bEditingSelect                    <- bEditing select

    liftIOLater $ onChange bModel $ \newModel -> runUI win $ do
        editingSelect <- liftIO $ currentValue bEditingSelect -- this work?
        let editing = editingSelect
        when (not editing) $ void $ do
            case unModel newModel of
                NotAsked -> do
                    child <- Lens.views starting string translations
                    element view # set children [child]
                Loading -> do
                    child <- Lens.views loading string translations
                    element view # set children [child]
                Failure e -> do
                    child <- Lens.views mainPageError string translations
                    element view # set children [child]
                Data data' -> do
                    select' <- UI.div #. "select" #+ [element select]
                    content <-
                        UI.div
                        #. "section"
                        #+ [ UI.div
                             #. "field is-horizontal"
                             #+ [ UI.div
                                  #. "field-body"
                                  #+ [ UI.div
                                     #. "field"
                                     #+ [UI.p #. "control" #+ [element select']]
                                     ]
                                ]
                           ]
                    element view # set
                        children
                        [content, photographees']

--------------------------------------------------------------------------------

    let eModel = rumors $ tModel
    _ <- onEvent eModel $ \m -> do
        void $ liftIO $ do
            case toJust (unModel m) of
                Nothing -> return ()
                Just i  -> do
                    --Location.writeLocationFile mLocationConfigFile (location i)
                    Grade.writeGrades mGradesFile (grades i)
                    return ()


    tabs'      <- mkElement "nav" #. "section" #+ [mkTabs env tabs]
    navigation <-
        mkElement "footer" #. "section" #+ [mkNavigation env translations tabs]

    void $ UI.getBody win # set children [tabs', view, navigation]
