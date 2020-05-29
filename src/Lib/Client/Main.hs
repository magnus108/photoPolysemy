module Lib.Client.Main
    ( mainSection
    , mkModel
    ) where

import Utils.Comonad

import           Data.Char
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

        let elems' = sortOn (\(a,b,c) -> fmap toLower (Photographee._name a)) $ toList elems
        (mkPhotographee env) <$> elems'


mkPhotographee :: Env -> (Photographee.Photographee, Bool, Photographee.Photographees) -> UI Element
mkPhotographee Env{..} (photographee, isCenter, photographees)
    | isCenter = do
        let name = Lens.view Photographee.name photographee
        UI.div #. "section" #+ [mkButton name name #. "button is-selected" # set (attr "disabled") "true"]
    | otherwise = do
        let name = Lens.view Photographee.name photographee
        button <- mkButton name name
        UI.on UI.click button $ \_ ->
            Photographee.writePhotographees mPhotographeesFile photographees
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

    let options = maybe [] (CLocation.mkGrades env) <$> bGrades
    select         <- UI.select # sink items options

    let eSelect   = CLocation.selectGrade <$> filterJust (selectionChange' select)

    let allEvents = concatenate' <$> unions' (eSelect :| [])



    let bPhotographees = photographees <<$>> bItem
        bPhotographeeIdent = Photographee.toIdent <<$>> bPhotographees

    input <- UI.input #. "input"
    val <- currentValue $ fromMaybe "" <$> (fromMaybe (Just "") <$> bPhotographeeIdent)
    element input # set value val
    
    bEditingInput              <- bEditing input
    liftIOLater $ onChange bPhotographeeIdent $ \ident' -> runUI win $ do
        editingInput  <- liftIO $ currentValue bEditingInput
        when (not editingInput) $ void $ do
            element input # set value (fromMaybe "" (fromMaybe (Just "") ident'))


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


    return ((getElement select, getElement input), superTide)


mainSection :: Env -> Window -> Translation -> Tabs -> Behavior Model -> UI ()
mainSection env@Env{..} win translations tabs bModel = do

    let bItem   = toJust <$> unModel <$> bModel
        bGrades = grades <<$>> bItem
        bDumpDir = maybe [] (dumpFilesCounter env win) <$> (dumpDir <<$>> bItem)
        bPhotographees = photographees <<$>> bItem
        bPhotographee = Photographee.toName <<$>> bPhotographees
        bPhotographeeIdent = Photographee.toIdent <<$>> bPhotographees


    dumpFilesCounter' <- UI.div #. "section" # sink items bDumpDir

    ((select, input), tModel) <- gradeItem env win translations bModel
    select' <- UI.div #. "select" #+ [element select]

    bEditingSelect                    <- bEditing select
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

    let photographees'' =  fmap concat $ (photographeesList env win) . photographees <<$>> bItem
    photographees' <- UI.div # sink items photographees''


    currentPhotographee <- UI.h1 #. "is-size-4" # sink text (fromMaybe "" <$> (fromMaybe (Just "") <$> bPhotographee))


    inputSection <- UI.div #. "section" # set children [input, currentPhotographee]
    let bView = mkView env translations inputSection dumpFilesCounter' selectSection photographees' <$> bModel

    view <- UI.div # sink item bView

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


mkView env translations xx x y z model =
        case unModel model of
            NotAsked -> do
                Lens.views starting string translations
            Loading -> do
                Lens.views loading string translations
            Failure e -> do
                Lens.views mainPageError string translations
            Data data' -> do
                UI.div # set children [xx,x,y,z]
