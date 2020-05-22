{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Location
    ( locationSection
    , mkModel
    )
where

import           Lib.Client.Widget
import           Control.Monad
import           Data.Maybe
import           Lib.Client.Utils
import qualified Control.Lens                  as Lens

import           Reactive.Threepenny
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI

import           Utils.Comonad
import qualified Utils.ListZipper              as ListZipper

import           Lib.Translation
import           Lib.Data
import qualified Lib.Grade                     as Grade
import           Lib.Tab
import qualified Lib.Location                  as Location
import           Lib.Client.Tab

import           Lib.Client.Element
import           Lib.App                        ( Env(..)
                                                , Files(..)
                                                )
import           Control.Concurrent.MVar        ( withMVar )


data Item = Item { location :: Location.LocationFile
                 , grades :: Grade.Grades
                 }

newtype Model = Model { unModel :: Data String Item }

mkModel :: Location.Model -> Grade.Model -> Model
mkModel location grades =
    Model $ Item <$> Location.unModel location <*> Grade._grades grades


gradeItem
    :: Env
    -> Window
    -> Translation
    -> Behavior (Maybe Grade.Grades)
    -> UI ((Element, Element, Element), Tidings (Maybe Grade.Grades)) --Tidings Grade.Grades)
gradeItem env win translations bItem = do

    input <- UI.input
    bEditingSelect <- bEditing input
    liftIOLater $ onChange bItem $ \s -> runUI win $ do
        editing <- liftIO $ currentValue bEditingSelect
        when (not editing) $ void $ do
            let string = maybe "" Grade.showGrade s
            element input # set value string

    select <- UI.select
    bEditingSelect <- bEditing select
    liftIOLater $ onChange bItem $ \s -> runUI win $ do
        editing <- liftIO $ currentValue bEditingSelect
        when (not editing) $ void $ do
            let options =  maybe [] (mkGrades env) s
            element select # set children [] #+ options

    button <- mkButton "insert" (Lens.view newGrade translations)

    let eClick = mkNewGrade <$ UI.click button
    let eInput = inputGrade <$> UI.valueChange input
    let eSelect = selectGrade <$> filterJust (selectionChange' select)

    let allEvents = concatenate' <$> unions' (eSelect :| [eInput, eClick])
    _

    let gg = fmap (<&>) bItem <@> allEvents
    let superTide = tidings bItem gg

    return ((getElement input, getElement select, getElement button), superTide)


locationSection
    :: Env -> Window -> Translation -> Tabs -> Behavior Model -> UI ()
locationSection env@Env {..} win translations tabs bModel = mdo
    ((input, select, button), tGradeItem) <- gradeItem env win translations bGrade

    view <- UI.div

    let bItem :: Behavior (Maybe Item)
        bItem = toJust <$> unModel <$> bModel

        bGrade :: Behavior (Maybe Grade.Grades)
        bGrade = grades <<$>> bItem

--------------------------------------------------------------------------------

    bEditingSelect <- bEditing select
    bEditingInput <- bEditing input

    liftIOLater $ onChange bModel $ \newModel -> runUI win $ do
        editingInput <- liftIO $ currentValue bEditingInput
        editingSelect <- liftIO $ currentValue bEditingSelect -- this work?
        let editing = editingInput || editingSelect
        when (not editing) $ void $ do
            case unModel newModel of
                NotAsked  -> do
                    child <- Lens.views starting string translations
                    element view # set children [child]
                Loading   -> do
                    child <- Lens.views loading string translations
                    element view # set children [child]
                Failure e -> do
                    child <- Lens.views locationPageError string translations
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
                                    #+ [UI.p #. "control" #+ [element input #. "input"]]
                                    , UI.div
                                    #. "field"
                                    #+ [UI.p #. "control" #+ [element select']]
                                    ]
                                ]
                        ]
                    locationFileSection <- locationFileView env translations (location data')
                    insertSection <- UI.div #. "section" #+ [element button]
                    element view # set children [locationFileSection, content, insertSection]

--------------------------------------------------------------------------------
-- fix det der maybe behavior
    let eGradeItemIn = rumors $ tGradeItem

    _ <- onEvent eGradeItemIn $ \e -> do
        case e of
            Nothing -> return ()
            Just i  -> void $ liftIO $ Grade.writeGrades mGradesFile i

    tabs'      <- mkElement "nav" #. "section" #+ [mkTabs env tabs]
    navigation <-
        mkElement "footer" #. "section" #+ [mkNavigation env translations tabs]

    void $ UI.getBody win # set children [tabs', view, navigation]

    UI.setFocus (getElement input) -- Can only do this if element exists and should not do this if not focus



locationFileView :: Env -> Translation -> Location.LocationFile -> UI Element
locationFileView Env {..} translations locationFile = do
    title_  <- UI.div #+ [Lens.views locationTitle string translations]
    content <- UI.div #+ [UI.string (Location.unLocationFile locationFile)]

    pick    <-
        mkFilePicker "locationFilePicker" (Lens.view pickLocation translations)
            $ \file -> when (file /= "") $ void $ Location.writeLocationFile
                  mLocationConfigFile
                  (Location.LocationFile file)

    make <-
        mkFileMaker "locationsPicker" (Lens.view newLocation translations)
            $ \file -> when (file /= "") $ void $ Location.writeLocationFile
                  mLocationConfigFile
                  (Location.LocationFile file)

    pickers <- UI.div #. "buttons has-addons" #+ [element pick, element make]

    open    <- mkOpenFile "open"
                          (Lens.view openLocation translations)
                          (Location.unLocationFile locationFile)

    UI.div #. "section" # set children [title_, content, pickers, open]



inputGrade :: String -> Grade.Grades -> Grade.Grades
inputGrade name grades = Grade.Grades
    (ListZipper.mapFocus (const (Grade.Grade name)) (Grade.unGrades grades))


mkNewGrade :: Grade.Grades -> Grade.Grades
mkNewGrade grades =
    Grade.Grades $ ListZipper.insert (Grade.unGrades grades) (Grade.Grade "")


selectGrade ::  Int -> Grade.Grades -> Grade.Grades
selectGrade selected grades =
        -- TODO this just wierd
    fromMaybe grades $ asum $ ListZipper.toNonEmpty $ ListZipper.iextend
    (\thisIndex grades' ->
        if selected == thisIndex then Just (Grade.Grades grades') else Nothing
    )
    (Grade.unGrades grades)



mkGrades :: Env -> Grade.Grades -> [UI Element]
mkGrades env (Grade.Grades grades') = do
    let elems = ListZipper.iextend
            (\index grades'' -> (index, grades' == grades'', extract grades'')
            ) grades'

    map (mkGrade env) (ListZipper.toList elems)



mkGrade :: Env -> (Int, Bool, Grade.Grade) -> UI Element
mkGrade Env {..} (thisIndex, isCenter, grade) = do
    let name   = Grade.unGrade grade
    let option = UI.option # set value (show thisIndex) # set text name
    if isCenter then option # set UI.selected True else option
