{-# LANGUAGE TemplateHaskell #-}
module Lib.Client.InsertPhotographee
    ( insertPhotographeeSection
    , mkModel
    , unModel
    , location
    ) where

import Lib.Data

import qualified Utils.ListZipper as ListZipper
import Utils.Comonad

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Translation
import Lib.Tab
import qualified Lib.Photographee as Photographee
import qualified Lib.Grade as Grade
import qualified Lib.Location as Location

import Lib.Client.Tab
import qualified Lib.Client.Location as CLocation
import Lib.Client.Utils

import Lib.App (Env(..))

import qualified Control.Lens as Lens

import           Reactive.Threepenny
import Lib.Client.Element


import Control.Lens hiding (children, set, (#), element)


-- OVERVEJ DER SKAL LAVES EN SPECIFIKT TIL BUILD
data Item = Item { _location :: Location.LocationFile
                 , _grades :: Grade.Grades
                 , _photographees :: Photographee.Photographees
                 }

makeLenses ''Item

newtype Model = Model { _unModel :: Data String Item }

makeLenses ''Model

mkModel :: Location.Model -> Grade.Model -> Photographee.Model -> Model
mkModel location' grades' photograhees =
    Model $ Item <$>
        Location.unModel location' <*> Grade._grades grades' <*>
                    (Lens.view Photographee.unModel photograhees)


---------------------------------------------------------------------------------
insertPhotographeeSection :: Env -> Window -> Translation -> Tabs -> Behavior Model -> UI ()
insertPhotographeeSection env@Env{..} win translations tabs bModel = do
    view' <- sinkModel env win translations bModel

    tabs' <- mkElement "nav" #. "section" #+ [mkTabs env translations tabs]
    navigation <- mkElement "footer" #. "section" #+ [mkNavigation env translations tabs]

    void $ UI.getBody win # set children [tabs', view', navigation]


mkCreate :: Env -> Window -> Translation -> UI Element
mkCreate _ _ translations = do
    button <- mkButton "mover" (Lens.view createPhotographee translations)
    return button


mkPhotographees :: Env -> Photographee.Photographees -> [UI Element]
mkPhotographees env (Photographee.Photographees photographees') = do
    let elems = ListZipper.iextend (\i photographees'' -> (i, photographees' == photographees'', extract photographees'')) photographees'
    map (mkPhotographeeListItem env) (ListZipper.toList elems)


mkPhotographeeListItem :: Env -> (Int, Bool, Photographee.Photographee) -> UI Element
mkPhotographeeListItem Env {..} (thisIndex, isCenter, photographee) = do
    let name   = Photographee.toName' photographee
    let option = UI.option # set value (show thisIndex) # set text name
    if isCenter then option # set UI.selected True else option



selectPhotographeeSection :: Env -> Window -> Translation -> Element -> Element -> Element -> Element -> Element -> Element -> Photographee.Photographees -> UI Element
selectPhotographeeSection env _ translations input inputIdent inputSys select button selectGrade photographees' = do
    _ <- element input # set value (Photographee.toName photographees')
    _ <- element select # set children [] #+ (mkPhotographees env photographees')
    _ <- element inputIdent # set value (Photographee.toIdent photographees')
    _ <- element inputSys # set value (Photographee.toSys photographees')

    content <-
        UI.div
        #. "section"
        #+ [ 

           UI.div
                #. "field is-horizontal"
                #+ [ UI.div #. "field-label is-normal"
                    #+ [UI.label #. "label" #+ [Lens.views gradePick string translations]]
                   , UI.div
                    #. "field-body"
                    #+ [ UI.div
                        #. "field"
                        #+ [ UI.p #. "control" #+ [UI.div #. "select" #+ [element selectGrade]]]
                       ]]
           ,UI.div
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
                    #+ [UI.label #. "label" #+ [Lens.views photographeeSys string translations]]
                   , UI.div
                    #. "field-body"
                    #+ [ UI.div
                        #. "field"
                        #+ [ UI.p
                           #. "control"
                           #+ [element inputSys #. "input"]
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

    newPhotographee <- mkCreate env win translations
    inputPhotographee <- UI.input
    inputPhotographeeIdent <- UI.input
    inputPhotographeeSys <- UI.input
    selectPhotographee <- UI.select
    bEditingInputPhotographee <- bEditing inputPhotographee
    bEditingInputPhotographeeIdent <- bEditing inputPhotographeeIdent
    bEditingInputPhotographeeSys <- bEditing inputPhotographeeSys
    bEditingSelectPhotographee <- bEditing selectPhotographee


    bEditingSelect  <- bEditing select


    liftIO $ do
        model <- currentValue bModel
        runUI win $ void $ do
            case _unModel model of
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
                    let options = CLocation.mkGrades env (_grades item')
                    _ <- element select # set children [] #+ options


                    _ <- case (extract (Photographee.unPhotographees (_photographees item'))) of
                                (Photographee.Unknown _) -> do
                                    runFunction  $ ffi "$(%1).removeAttr('disabled')" (inputPhotographee)
                                    runFunction  $ ffi "$(%1).removeAttr('disabled')" (inputPhotographeeIdent)
                                    runFunction  $ ffi "$(%1).removeAttr('disabled')" (inputPhotographeeSys)
                                (Photographee.Known _ ) -> do
                                    void $ element inputPhotographee # set (attr "disabled") "true"
                                    void $ element inputPhotographeeIdent # set (attr "disabled") "true"
                                    void $ element inputPhotographeeSys # set (attr "disabled") "true"

                    selectInputPhotographeeSection <- selectPhotographeeSection env win translations inputPhotographee inputPhotographeeIdent inputPhotographeeSys selectPhotographee newPhotographee select (_photographees item')
                    _ <- element content # set children [selectInputPhotographeeSection]
                    return ()


    liftIO $ onChange bModel $ \model -> runUI win $ do
        case _unModel model of
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
                editingInputPhotographeeSys <- liftIO $ currentValue bEditingInputPhotographeeSys
                editingSelectPhotographee <- liftIO $ currentValue bEditingSelectPhotographee

                _ <- case (extract (Photographee.unPhotographees (_photographees item'))) of
                            (Photographee.Unknown _) -> do
                                runFunction  $ ffi "$(%1).removeAttr('disabled')" (inputPhotographee)
                                runFunction  $ ffi "$(%1).removeAttr('disabled')" (inputPhotographeeIdent)
                                runFunction  $ ffi "$(%1).removeAttr('disabled')" (inputPhotographeeSys)
                            (Photographee.Known _ ) -> do
                                void $ element inputPhotographee # set (attr "disabled") "true"
                                void $ element inputPhotographeeIdent # set (attr "disabled") "true"
                                void $ element inputPhotographeeSys # set (attr "disabled") "true"

                when (not editingInputPhotographee ) $ void $
                    element inputPhotographee # set value (Photographee.toName (_photographees item'))

                when (not editingSelectPhotographee) $ void $
                    element selectPhotographee # set children [] #+ (mkPhotographees env (_photographees item'))

                when (not editingInputPhotographeeIdent) $ void $
                    element inputPhotographeeIdent # set value (Photographee.toIdent (_photographees item'))

                when (not editingInputPhotographeeSys) $ void $
                    element inputPhotographeeSys # set value (Photographee.toSys (_photographees item'))

                editingSelect <- liftIO $ currentValue bEditingSelect

                when (not editingSelect) $ void $ do
                    let options = CLocation.mkGrades env (_grades item')
                    element select # set children [] #+ options


                when (not (editingSelectPhotographee || editingSelect || editingInputPhotographee || editingInputPhotographeeIdent  || editingInputPhotographeeSys)) $ void $ do
                    selectInputPhotographeeSection <- selectPhotographeeSection env win translations inputPhotographee inputPhotographeeIdent inputPhotographeeSys selectPhotographee newPhotographee select (_photographees item')
                    _ <- element content # set children [ selectInputPhotographeeSection] 
                    return ()


    let eNewPhotographee = Photographee.insert (Photographee.Unknown Photographee.empty) <$ UI.click newPhotographee
    let eInputPhotographee = Photographee.setName <$> UI.valueChange inputPhotographee
    let eInputPhotographeeIdent = Photographee.setIdent <$> UI.valueChange inputPhotographeeIdent
    let eInputPhotographeeSys = Photographee.setSys <$> UI.valueChange inputPhotographeeSys

    let eSelect   = selectPhotographeeF <$> filterJust (selectionChange' selectPhotographee)

    let allEventsPhotographee = concatenate' <$> unions' (eInputPhotographee :| [eNewPhotographee, eInputPhotographeeSys, eInputPhotographeeIdent, eSelect])

    let eSelectGrade = CLocation.selectGrade <$> filterJust (selectionChange' select)

    let gradeEvent = concatenate' <$> unions' (eSelectGrade :| [])
    let ee  = filterJust
                $   fmap
                        (\m f -> case toJust (_unModel m) of
                            Nothing -> Nothing
                            Just x -> Just $ Model $ Data $ Lens.over grades f x
                        )
                        bModel
                <@> gradeEvent


    let ee3 = filterJust
                $   fmap
                        (\m f -> case toJust (_unModel m) of
                            Nothing -> Nothing
                            Just x -> Just $ Model $ Data $ Lens.over photographees f x
                        )
                        bModel
                <@> allEventsPhotographee



    _ <- onEvent ee3 $ \model -> do
        void $ liftIO $ do
            case toJust (_unModel model) of
                Nothing -> return ()
                Just item'  -> do
                    --Location.writeLocationFile mLocationConfigFile (location i)
                    _ <- Photographee.writePhotographees mPhotographeesFile (_photographees item')
                    return ()


    _ <- onEvent ee $ \model -> do
        void $ liftIO $ do
            case toJust (_unModel model) of
                Nothing -> return ()
                Just item'  -> do
                    --Location.writeLocationFile mLocationConfigFile (location i)
                    _ <- Grade.writeGrades mGradesFile (_grades item')
                    return ()

    return content
