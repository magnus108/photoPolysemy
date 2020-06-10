module Lib.Client.Main
    ( mainSection
    , mkModel
    ) where

import Lib.Data
import qualified Lib.Main as Main
import qualified Lib.Server.Build as SBuild
import qualified Lib.Build as Build
import qualified Utils.RoseTree as RT
import qualified Utils.TreeZipper as TZ

import qualified Utils.ListZipper as ListZipper
import Utils.Comonad

import           Data.Char
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Translation
import Lib.Tab
import qualified Lib.Photographer as Photographer
import qualified Lib.DagsdatoBackup as DagsdatoBackup
import qualified Lib.Doneshooting as Doneshooting
import qualified Lib.Shooting as Shooting
import qualified Lib.Photographee as Photographee
import qualified Lib.Grade as Grade
import qualified Lib.Dump as Dump
import qualified Lib.Session as Session
import qualified Lib.Location as Location
import qualified Lib.Camera as Camera
import qualified Lib.Dagsdato as Dagsdato

import Lib.Client.Tab
import qualified Lib.Client.Location as CLocation
import Lib.Client.Utils

import Lib.App (Env(..))

import qualified Control.Lens as Lens

import           Reactive.Threepenny
import Lib.Client.Element


mkModel :: Location.Model -> Grade.Model -> Dump.DumpModel -> Dump.DumpDirModel -> Photographee.Model -> Data String Session.Session -> Data String Camera.Camera -> Data String Dagsdato.Dagsdato -> Data String Shooting.Shooting -> Data String Doneshooting.Doneshooting -> Data String Photographer.Photographer -> Data String DagsdatoBackup.DagsdatoBackup -> Data String Build.Build -> Main.Model
mkModel location grades dump dumpDir photograhees session camera dagsdato shooting doneshooting photographer dagsdatoBackup build =
    Main.Model $ Main.Item <$>
        Location.unModel location <*> Grade._grades grades <*>
            Dump.unModel dump <*>
                Dump.unDumpDirModel dumpDir <*>
                    (Lens.view Photographee.unModel photograhees)
                    <*> session
                    <*> camera
                    <*> dagsdato
                    <*> shooting
                    <*> doneshooting
                    <*> photographer
                    <*> dagsdatoBackup
                    <*> build


photographeesList :: Env -> Window -> Photographee.Photographees -> UI [Element]
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
        mapM (mkPhotographee env) elems'


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
dumpFilesCounter _ _ translations dumpDir =
    UI.div #. "section" #+
        [ mkLabel (Lens.view dumpDirCounter translations)
        , UI.string (show $ Dump.count dumpDir)
                #. "is-size-1 has-text-danger has-text-weight-bold" # set (attr "id") "count"]



---------------------------------------------------------------------------------
mainSection :: Env -> Window -> Translation -> Tabs -> Behavior Main.Model -> UI ()
mainSection env@Env{..} win translations tabs bModel = do
    view <- sinkModel env win translations bModel

    tabs' <- mkElement "nav" #. "section" #+ [mkTabs env translations tabs]
    navigation <- mkElement "footer" #. "section" #+ [mkNavigation env translations tabs]

    void $ UI.getBody win # set children [tabs', view, navigation]


mkCreate :: Env -> Window -> Translation -> UI Element
mkCreate _ _ translations = do
    button <- mkButton "mover" (Lens.view createPhotographee translations)
    return button


mkPhotographees :: Env -> Photographee.Photographees -> [UI Element]
mkPhotographees env (Photographee.Photographees photographees') = do
    let elems = ListZipper.iextend (\index photographees'' -> (index, photographees' == photographees'', extract photographees'')) photographees'
    map (mkPhotographeeListItem env) (ListZipper.toList elems)


mkPhotographeeListItem :: Env -> (Int, Bool, Photographee.Photographee) -> UI Element
mkPhotographeeListItem Env {..} (thisIndex, isCenter, photographee) = do
    let name   = Lens.view Photographee.name photographee
    let option = UI.option # set value (show thisIndex) # set text name
    if isCenter then option # set UI.selected True else option



selectPhotographeeSection :: Env -> Window -> Translation -> Element -> Element -> Element -> Element -> Photographee.Photographees -> UI Element
selectPhotographeeSection env _ translations input inputIdent select button photographees = do
    _ <- element input # set value (Photographee.toName photographees)
    _ <- element select # set children [] #+ (mkPhotographees env photographees)
    _ <- element inputIdent # set value (Photographee.toIdent photographees)
    content <-
        UI.div
        #. "section"
        #+ [ 
        UI.div
                #. "field is-horizontal"
                #+ [ UI.div #. "field-label is-normal"
                    #+ [UI.label #. "label" #+ [Lens.views photographeeName string translations]]
                   , UI.div
                    #. "field-body"
                    #+ [ UI.div
                        #. "field"
                        #+ [ UI.p
                            #. "control"
                            #+ [element input #. "input"]
                        ]
                ]
                ]
           , UI.div
                #. "field is-horizontal"
                #+ [ UI.div #. "field-label is-normal"
                    #+ [UI.label #. "label" #+ [Lens.views photographeeIdent string translations]]
                   , UI.div
                    #. "field-body"
                    #+ [ UI.div
                        #. "field"
                        #+ [ UI.p
                           #. "control"
                           #+ [element inputIdent #. "input"]
                        ]
                        ]
                ]
           , UI.div
                #. "field is-horizontal"
                #+ [ UI.div #. "field-label is-normal"
                    #+ [UI.label #. "label" #+ [Lens.views photographeePick string translations]]
                   , UI.div
                    #. "field-body"
                    #+ [ UI.div
                        #. "field"
                        #+ [ UI.p #. "control" #+ [UI.div #. "select" #+ [element select]]]
                       ]]

           , UI.div
                #. "field is-horizontal"
                #+ [ UI.div #. "field-label is-normal"
                    #+ [UI.label #. "label"]
                   , UI.div
                    #. "field-body"
                    #+ [ UI.div
                        #. "field"
                        #+ [ UI.p #. "control" #+ [element button]]
                       ]]
            ]

    return content

selectPhotographeeF :: Int -> Photographee.Photographees -> Photographee.Photographees
selectPhotographeeF selected (Photographee.Photographees photographees') =
        -- TODO this just wierd
    fromMaybe (Photographee.Photographees photographees') $ asum $ ListZipper.toNonEmpty $ ListZipper.iextend
        (\thisIndex photographees'' -> if selected == thisIndex
            then Just (Photographee.Photographees photographees'')
            else Nothing
        ) photographees'


setBuild :: Env -> Translation -> Element -> Session.Session -> UI ()
setBuild _ translations button session = do
    let name = Session.translationSessionButton session translations
    void $ element button # set text name



sinkModel :: Env -> Window -> Translation -> Behavior Main.Model -> UI Element
sinkModel env@Env{..} win translations bModel = do

    mkBuild' <- mkButton "mkBuild" ""
    mkBuild <- UI.div #. "section" # set children [mkBuild']

    build <- UI.div #. "section"

    content <- UI.div
    select <- UI.select
    input <- UI.input #. "input"
    currentPhotographee <- UI.h1 #. "is-size-4"
    inputSection <- UI.div #. "section" # set children [input, currentPhotographee]

    selectSection <-
        UI.div
        #. "section"
        #+ [ UI.div
                #. "field is-horizontal"
                #+ [ UI.div
                    #. "field-body"
                    #+ [ UI.div
                        #. "field"
                        #+ [UI.p #. "control" #+ [UI.div #. "select" #+ [element select]]]
                        ]
                ]
            ]

    newPhotographee <- mkCreate env win translations
    inputPhotographee <- UI.input
    inputPhotographeeIdent <- UI.input
    selectPhotographee <- UI.select
    bEditingInputPhotographee <- bEditing inputPhotographee
    bEditingInputPhotographeeIdent <- bEditing inputPhotographeeIdent
    bEditingSelectPhotographee <- bEditing selectPhotographee

    photographees' <- UI.div


    bEditingInput <- bEditing input
    bEditingSelect  <- bEditing select


    liftIOLater $ do
        model <- currentValue bModel
        runUI win $ void $ do
            case Main._unModel model of
                NotAsked -> do
                    msg <- Lens.views starting string translations
                    _ <- element content # set children [msg]
                    return ()
                Loading -> do
                    msg <- Lens.views loading string translations
                    _ <- element content # set children [msg]
                    return ()
                Failure e -> do
                    msg <- Lens.views mainPageError string translations
                    err <- UI.div #+ [string e]
                    section <- UI.div #. "section" # set children [msg, err]
                    _ <- element content # set children [section]
                    return ()

                Data item' -> do
                    buildStatus <- UI.string $ Build.toString (Main._build item') translations
                    _ <- element build # set children [buildStatus]
                    _ <- setBuild env translations mkBuild' (Main._session item')
                    selectInputPhotographeeSection <- selectPhotographeeSection env win translations inputPhotographee inputPhotographeeIdent selectPhotographee newPhotographee (Main._photographees item')
                    
                    dumpFilesCounter' <- dumpFilesCounter env win translations (Main._dumpDir item')
                    let options = CLocation.mkGrades env (Main._grades item')
                    _ <- element select # set children [] #+ options

                    photographeesList' <- photographeesList env win (Main._photographees item')
                    _ <- element photographees' # set children photographeesList'

                    let ident = Photographee.toIdent (Main._photographees item')
                    let name = Photographee.toName (Main._photographees item')
                    _ <- element currentPhotographee # set text name
                    _ <- element input # set value ident
                    _ <- element content # set children [build, mkBuild, dumpFilesCounter', inputSection, selectInputPhotographeeSection, selectSection, photographees']
                    return ()


    liftIOLater $ onChange bModel $ \model -> runUI win $ do
        case Main._unModel model of
            NotAsked -> do
                msg <- Lens.views starting string translations
                _ <- element content # set children [msg]
                return ()
            Loading -> do
                msg <- Lens.views loading string translations
                _ <- element content # set children [msg]
                return ()
            Failure e -> do
                msg <- Lens.views mainPageError string translations
                err <- UI.div #+ [string e]
                section <- UI.div #. "section" # set children [msg, err]
                _ <- element content # set children [section]
                return ()
            Data item' -> do
                editingInputPhotographee <- liftIO $ currentValue bEditingInputPhotographee
                editingInputPhotographeeIdent <- liftIO $ currentValue bEditingInputPhotographeeIdent
                editingSelectPhotographee <- liftIO $ currentValue bEditingSelectPhotographee


                when (not editingInputPhotographee ) $ void $
                    element inputPhotographee # set value (Photographee.toName (Main._photographees item'))

                when (not editingSelectPhotographee) $ void $
                    element selectPhotographee # set children [] #+ (mkPhotographees env (Main._photographees item'))

                when (not editingInputPhotographeeIdent) $ void $
                    element inputPhotographeeIdent # set value (Photographee.toIdent (Main._photographees item'))

                editingInput <- liftIO $ currentValue bEditingInput
                editingSelect <- liftIO $ currentValue bEditingSelect

                buildStatus <- UI.string $ Build.toString (Main._build item') translations
                _ <- element build # set children [buildStatus]

                dumpFilesCounter' <- dumpFilesCounter env win translations (Main._dumpDir item')
                photographeesList' <- photographeesList env win (Main._photographees item')
                _ <- element photographees' # set children photographeesList'

                let ident = Photographee.toIdent (Main._photographees item')
                let name = Photographee.toName (Main._photographees item')
                _ <- element currentPhotographee # set text name

                _ <- setBuild env translations mkBuild' (Main._session item')

                when (not editingSelect) $ void $ do
                    let options = CLocation.mkGrades env (Main._grades item')
                    element select # set children [] #+ options

                when (not editingInput) $ void $
                    element input # set value ident --- eh

                when (not (editingInput || editingSelectPhotographee || editingSelect || editingInputPhotographee || editingInputPhotographeeIdent )) $ void $ do
                    selectInputPhotographeeSection <- selectPhotographeeSection env win translations inputPhotographee inputPhotographeeIdent selectPhotographee newPhotographee (Main._photographees item')
                    _ <- element content # set children [build ,mkBuild, dumpFilesCounter', inputSection, selectInputPhotographeeSection, selectSection, photographees']
                    return ()


    let eNewPhotographee = Photographee.insert Photographee.empty <$ UI.click newPhotographee
    let eInputPhotographee = Photographee.setName <$> UI.valueChange inputPhotographee
    let eInputPhotographeeIdent = Photographee.setIdent <$> UI.valueChange inputPhotographeeIdent

    let eSelect   = selectPhotographeeF <$> filterJust (selectionChange' selectPhotographee)

    let allEventsPhotographee = concatenate' <$> unions' (eInputPhotographee :| [eNewPhotographee, eInputPhotographeeIdent, eSelect])

    let eSelectGrade = CLocation.selectGrade <$> filterJust (selectionChange' select)
    let eFind = Photographee.tryFindById <$> UI.valueChange input

    let gradeEvent = concatenate' <$> unions' (eSelectGrade :| [])
    let findEvent = concatenate' <$> unions' (eFind :| [])
    let ee  = filterJust
                $   fmap
                        (\m f -> case toJust (Main._unModel m) of
                            Nothing -> Nothing
                            Just x -> Just $ Main.Model $ Data $ Lens.over Main.grades f x
                        )
                        bModel
                <@> gradeEvent


    let ee2 = filterJust
                $   fmap
                        (\m f -> case toJust (Main._unModel m) of
                            Nothing -> Nothing
                            Just x -> Just $ Main.Model $ Data $ Lens.over Main.photographees f x
                        )
                        bModel
                <@> findEvent

    let ee3 = filterJust
                $   fmap
                        (\m f -> case toJust (Main._unModel m) of
                            Nothing -> Nothing
                            Just x -> Just $ Main.Model $ Data $ Lens.over Main.photographees f x
                        )
                        bModel
                <@> allEventsPhotographee

    let ee4 = filterJust
                $   fmap
                        (\m -> case toJust (Main._unModel m) of
                            Nothing -> Nothing
                            Just x -> Just (Main.Model (Data x))
                        )
                        bModel
                <@ UI.click mkBuild'

    _ <- onEvent ee4 $ \model -> do
        void $ liftIO $ do
            case toJust (Main._unModel model) of
                Nothing -> return ()
                Just item'  -> do
                    SBuild.entry mBuildFile item' 
                    return ()


    _ <- onEvent ee3 $ \model -> do
        void $ liftIO $ do
            case toJust (Main._unModel model) of
                Nothing -> return ()
                Just item'  -> do
                    --Location.writeLocationFile mLocationConfigFile (location i)
                    _ <- Photographee.writePhotographees mPhotographeesFile (Main._photographees item')
                    return ()

    _ <- onEvent ee2 $ \model -> do
        void $ liftIO $ do
            case toJust (Main._unModel model) of
                Nothing -> return ()
                Just item'  -> do
                    --Location.writeLocationFile mLocationConfigFile (location i)
                    _ <- Photographee.writePhotographees mPhotographeesFile (Main._photographees item')
                    return ()

    _ <- onEvent ee $ \model -> do
        void $ liftIO $ do
            case toJust (Main._unModel model) of
                Nothing -> return ()
                Just item'  -> do
                    --Location.writeLocationFile mLocationConfigFile (location i)
                    _ <- Grade.writeGrades mGradesFile (Main._grades item')
                    return ()

    return content
