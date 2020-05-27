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
                 , photographees :: Photographee.Photographees
                 }

newtype Model = Model { unModel :: Data String Item }

mkModel :: Location.Model -> Grade.Model -> Dump.DumpModel -> Dump.DumpDirModel -> Photographee.Model -> Model
mkModel location grades dump dumpDir photograhees =
    Model $ Item <$> Location.unModel location <*> Grade._grades grades <*> Dump.unModel dump <*> Dump.unDumpDirModel dumpDir <*> (Lens.view Photographee.unModel photograhees)


photographeesList :: Env -> Window -> Photographee.Photographees -> [UI Element]
photographeesList env win Photographee.NoPhotographees = []
photographeesList env win (Photographee.Photographees photographees') = do
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
        --    writePhotographees mPhotographersFile photographers
            return ()
        UI.div #. "section" #+ [element button]

dumpFilesCounter :: Env -> Window -> Dump.DumpDir -> [UI Element]
dumpFilesCounter env window (Dump.DumpDir  dumpDir) =
    [ mkLabel "Antal billeder i dump:", UI.string (show $ length (dumpDir)) #. "is-size-1 has-text-danger has-text-weight-bold" # set (attr "id") "count"]


gradeItem
    :: Env
    -> Window
    -> Translation
    -> Behavior Model
    -> UI ((Element, Element), Tidings Model)
gradeItem env win translations bModel = do
    let bItem   = toJust <$> unModel <$> bModel
        bGrades = grades <<$>> bItem

    select         <- UI.select

    let eSelect   = CLocation.selectGrade <$> filterJust (selectionChange' select)

    let allEvents = concatenate' <$> unions' (eSelect :| [])

    photograhees' <- UI.div

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
                                        (Data (Item (location x) (f (grades x)) (dump x) (dumpDir x) (photographees x)))
                                    )
                        )
                        bModel
                <@> allEvents


    return ((getElement select, getElement photograhees'), superTide)


mainSection :: Env -> Window -> Translation -> Tabs -> Behavior Model -> UI ()
mainSection env@Env{..} win translations tabs bModel = do
    view                              <- UI.div

    dumpFilesCounter' <- UI.div
    ((select, photographees'), tModel) <- gradeItem env win translations bModel
    select' <- UI.div #. "select" #+ [element select]

    bEditingSelect                    <- bEditing select

    childStarting <- Lens.views starting string translations
    childLoading <- Lens.views loading string translations
    childErr <- UI.div

    selectSection <-
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


    liftIOLater $ onChange bModel $ \newModel -> runUI win $ do
        editingSelect <- liftIO $ currentValue bEditingSelect -- this work?

        let dumpFiles' = fmap dumpDir $ toJust $ unModel newModel
        element dumpFilesCounter' #. "section" # set children [] #+ maybe [] (dumpFilesCounter env win) dumpFiles'

        let photographees'' = fmap photographees $ toJust $ unModel newModel
        element photographees' # set children [] #+ maybe [] (photographeesList env win) photographees''

        when (not editingSelect) $ void $ do
            let grades' = fmap grades $ toJust $ unModel newModel
            let options = maybe [] (CLocation.mkGrades env) grades'
            element select # set children [] #+ options

        when (not editingSelect) $ void $ do
            case unModel newModel of
                NotAsked -> do
                    element view # set children [childStarting]
                Loading -> do
                    element view # set children [childLoading]
                Failure e -> do
                    element childErr # set text (Lens.view mainPageError translations)
                    element view # set children [childErr]
                Data data' -> do
                    element view # set children [dumpFilesCounter', selectSection, photographees']

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
