module Lib.Client.Main
    ( mainSection
    , mkModel
    ) where

import qualified Utils.ListZipper as ListZipper
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


photographeesList :: Env -> Window -> Photographee.Photographees -> UI [Element]
photographeesList _ _ Photographee.NoPhotographees = return []
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


mkCreate :: Env -> Window -> Translation -> UI Element
mkCreate env win translations = do
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
    _ <- element input # set value (fromMaybe "" (Photographee.toName photographees))
    _ <- element select # set children [] #+ (mkPhotographees env photographees)
    _ <- element inputIdent # set value (fromMaybe "" (Photographee.toIdent photographees))
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



sinkModel :: Env -> Window -> Translation -> Behavior Model -> UI Element
sinkModel env@Env{..} win translations bModel = do

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
                    selectInputPhotographeeSection <- selectPhotographeeSection env win translations inputPhotographee inputPhotographeeIdent selectPhotographee newPhotographee (photographees item)
                    
                    editing <- liftIO $ currentValue bEditingInput
                    dumpFilesCounter' <- dumpFilesCounter env win translations (dumpDir item)
                    let options = CLocation.mkGrades env (grades item)
                    _ <- element select # set children [] #+ options

                    photographeesList' <- photographeesList env win (photographees item)
                    _ <- element photographees' # set children photographeesList'

                    let ident = Photographee.toIdent (photographees item)
                    let name = Photographee.toName (photographees item)
                    element currentPhotographee # set text (fromMaybe "" name)
                    _ <- element input # set value (fromMaybe "" ident) --- eh
                    element content # set children [dumpFilesCounter', inputSection, selectInputPhotographeeSection, selectSection, photographees']
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
                editingInputPhotographee <- liftIO $ currentValue bEditingInputPhotographee
                editingInputPhotographeeIdent <- liftIO $ currentValue bEditingInputPhotographeeIdent
                editingSelectPhotographee <- liftIO $ currentValue bEditingSelectPhotographee


                when (not editingInputPhotographee ) $ void $
                    element inputPhotographee # set value (fromMaybe "" (Photographee.toName (photographees item)))

                when (not editingSelectPhotographee) $ void $
                    element selectPhotographee # set children [] #+ (mkPhotographees env (photographees item))

                when (not editingInputPhotographeeIdent) $ void $
                    element inputPhotographeeIdent # set value (fromMaybe "" (Photographee.toIdent (photographees item)))

                editingInput <- liftIO $ currentValue bEditingInput
                editingSelect <- liftIO $ currentValue bEditingSelect

                dumpFilesCounter' <- dumpFilesCounter env win translations (dumpDir item)
                photographeesList' <- photographeesList env win (photographees item)
                _ <- element photographees' # set children photographeesList'

                let ident = Photographee.toIdent (photographees item)
                let name = Photographee.toName (photographees item)
                element currentPhotographee # set text (fromMaybe "" name)

                when (not editingSelect) $ void $ do
                    let options = CLocation.mkGrades env (grades item)
                    element select # set children [] #+ options

                when (not editingInput) $ void $
                    element input # set value (fromMaybe "" ident) --- eh

                when (not (editingInput || editingSelectPhotographee || editingSelect || editingInputPhotographee || editingInputPhotographeeIdent )) $ void $ do
                    selectInputPhotographeeSection <- selectPhotographeeSection env win translations inputPhotographee inputPhotographeeIdent selectPhotographee newPhotographee (photographees item)
                    element content # set children [dumpFilesCounter', inputSection, selectInputPhotographeeSection, selectSection, photographees']
                    return ()


    let eNewPhotographee = Photographee.insert Photographee.empty <$ UI.click newPhotographee
    let eInputPhotographee = Photographee.setName <$> UI.valueChange inputPhotographee
    let eInputPhotographeeIdent = Photographee.setIdent <$> UI.valueChange inputPhotographeeIdent

    let eSelect   = selectPhotographeeF <$> filterJust (selectionChange' selectPhotographee)

    let allEventsPhotographee = concatenate' <$> unions' (eInputPhotographee :| [eNewPhotographee, eInputPhotographeeIdent, eSelect])

    let eSelect   = CLocation.selectGrade <$> filterJust (selectionChange' select)
    let eFind = Photographee.tryFindById <$> UI.valueChange input

    let gradeEvent = concatenate' <$> unions' (eSelect :| [])
    let findEvent = concatenate' <$> unions' (eFind :| [])
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
                <@> gradeEvent


    let ee2 = filterJust
                $   fmap
                        (\m f -> case toJust (unModel m) of
                            Nothing -> Nothing
                            Just x ->
                                Just
                                    (Model
                                        (Data (Item (location x) (grades x) (dump x) (dumpDir x) (f (photographees x))))
                                    )
                        )
                        bModel
                <@> findEvent

    let ee3 = filterJust
                $   fmap
                        (\m f -> case toJust (unModel m) of
                            Nothing -> Nothing
                            Just x ->
                                Just
                                    (Model
                                        (Data (Item (location x) (grades x) (dump x) (dumpDir x) (f (photographees x))))
                                    )
                        )
                        bModel
                <@> allEventsPhotographee

    _ <- onEvent ee3 $ \model -> do
        void $ liftIO $ do
            case toJust (unModel model) of
                Nothing -> return ()
                Just item'  -> do
                    --Location.writeLocationFile mLocationConfigFile (location i)
                    _ <- Photographee.writePhotographees mPhotographeesFile (photographees item')
                    return ()

    _ <- onEvent ee2 $ \model -> do
        void $ liftIO $ do
            case toJust (unModel model) of
                Nothing -> return ()
                Just item'  -> do
                    --Location.writeLocationFile mLocationConfigFile (location i)
                    _ <- Photographee.writePhotographees mPhotographeesFile (photographees item')
                    return ()

    _ <- onEvent ee $ \model -> do
        void $ liftIO $ do
            case toJust (unModel model) of
                Nothing -> return ()
                Just item'  -> do
                    --Location.writeLocationFile mLocationConfigFile (location i)
                    _ <- Grade.writeGrades mGradesFile (grades item')
                    return ()

    return content
