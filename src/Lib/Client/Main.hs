module Lib.Client.Main
    ( mainSection
    , mkModel
    ) where

import Utils.Comonad

import           Data.Char
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

import Lib.App (Env(..))

import qualified Control.Lens as Lens

import           Reactive.Threepenny
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
photographeesList _ _ Photographee.NoPhotographees = []
photographeesList env _ (Photographee.Photographees photographees') = do
        let currentPhotographee = extract photographees'
        let elems = photographees' =>> \photographees''-> let
                        thisPhotographee = extract photographees''
                    in
                        ( thisPhotographee
                        , thisPhotographee == currentPhotographee
                        , Photographee.Photographees photographees''
                        )

        let elems' = sortOn (\(a,_,_) -> fmap toLower (Photographee._name a)) $ toList elems
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



dumpFilesCounter :: Env -> Window -> Translation -> Dump.DumpDir -> UI Element
dumpFilesCounter _ _ translations (Dump.DumpDir dumpDir) =
    UI.div #. "section" #+
        [ mkLabel (Lens.view dumpDirCounter translations)
        , UI.string (show $ length (dumpDir))
                #. "is-size-1 has-text-danger has-text-weight-bold" # set (attr "id") "count"]



---------------------------------------------------------------------------------
mainSection :: Env -> Window -> Translation -> Tabs -> Behavior Model -> UI ()
mainSection env@Env{..} win translations tabs bModel = do
    view <- sinkModel env win translations bModel

    tabs' <- mkElement "nav" #. "section" #+ [mkTabs env tabs]
    navigation <- mkElement "footer" #. "section" #+ [mkNavigation env translations tabs]

    void $ UI.getBody win # set children [tabs', view, navigation]



mkSelect env grades' select = do
    let options = CLocation.mkGrades env grades'
    _ <- element select # set children [] #+ options
    select' <- UI.div #. "select" #+ [element select]
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
    return selectSection


mkInput env photographees' input = do
    let ident = Photographee.toIdent photographees'
    let name = Photographee.toName photographees'
    _ <- element input # set value (fromMaybe "" ident) --- eh
    currentPhotographee <- UI.h1 #. "is-size-4" # set text (fromMaybe "" name)
    inputSection <- UI.div #. "section" # set children [input, currentPhotographee]
    return inputSection


sinkModel :: Env -> Window -> Translation -> Behavior Model -> UI Element
sinkModel env@Env{..} win translations bModel = do

    content <- UI.div
    select <- UI.select
    input <- UI.input #. "input"
    bEditingInput <- bEditing input

    liftIOLater $ do
        model <- currentValue bModel
        runUI win $ void $ do
            case unModel model of
                NotAsked -> do
                    msg <- Lens.views starting string translations
                    element content # set children [msg]
                    return ()
                Loading -> do
                    msg <- Lens.views loading string translations
                    element content # set children [msg]
                    return ()
                Failure _ -> do
                    msg <- Lens.views mainPageError string translations
                    element content # set children [msg]
                    return ()

                Data item -> do
                    dumpFilesCounter' <- dumpFilesCounter env win translations (dumpDir item)
                    selectSection <- mkSelect env (grades item) select
                    inputSection <- mkInput env (photographees item) input
                    let photographees'' =  photographeesList env win (photographees item)
                    photographees' <- UI.div #+ photographees''
                    element content # set children [dumpFilesCounter', inputSection, selectSection, photographees']
                    return ()


    liftIOLater $ onChange bModel $ \model -> runUI win $ do
        case unModel model of
            NotAsked -> do
                msg <- Lens.views starting string translations
                element content # set children [msg]
                return ()
            Loading -> do
                msg <- Lens.views loading string translations
                element content # set children [msg]
                return ()
            Failure _ -> do
                msg <- Lens.views mainPageError string translations
                element content # set children [msg]
                return ()
            Data item -> do
                editing <- liftIO $ currentValue bEditingInput
                when (not editing) $ void $ do
                    dumpFilesCounter' <- dumpFilesCounter env win translations (dumpDir item)
                    selectSection <- mkSelect env (grades item) select
                    inputSection <- mkInput env (photographees item) input
                    let photographees'' =  photographeesList env win (photographees item)
                    photographees' <- UI.div #+ photographees''
                    element content # set children [dumpFilesCounter', inputSection, selectSection, photographees']
                    return ()


    let eSelect   = CLocation.selectGrade <$> filterJust (selectionChange' select)
    let eFind = filterJust $ Photographee.findById <$> UI.valueChange input
    let allEvents = concatenate' <$> unions' (eSelect :| [])
    let ee  = filterJust
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

    _ <- onEvent ee $ \model -> do
        void $ liftIO $ do
            case toJust (unModel model) of
                Nothing -> return ()
                Just item'  -> do
                    --Location.writeLocationFile mLocationConfigFile (location i)
                    _ <- Grade.writeGrades mGradesFile (grades item')
                    return ()

    return content
