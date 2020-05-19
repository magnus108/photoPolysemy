{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Location
    ( locationSection
    ) where


import Control.Monad
import Data.Maybe
import Lib.Client.Utils
import qualified Control.Lens as Lens

import Reactive.Threepenny
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Utils.Comonad
import qualified Utils.ListZipper as ListZipper

import Lib.Translation
import Lib.Data
import qualified Lib.Grade as Grade
import Lib.Tab
import qualified Lib.Location as Location
import Lib.Client.Tab

import Lib.Client.Element
import Lib.App (Env(..), Files(..))
import Control.Concurrent.MVar (withMVar)


data Item = Item { location :: Location.LocationFile
                 , grades :: Grade.Grades
                 }

newtype Model = Model { unModel :: Data String Item }

mkModel :: Location.Model -> Grade.Model -> Model
mkModel location grades = Model $ Item <$> Location.unModel location <*> Grade._grades grades



    -- kun fordi ikke noget initial event.
mkLocation :: Env -> Window -> Translation -> Element -> Element -> Element -> Model -> UI Element
mkLocation Env{..} win translations gradeSelect gradeInput gradeInsert model = do
    case unModel model of
        NotAsked ->
            UI.div #+ [Lens.views starting string translations]
        Loading ->
            UI.div #+ [Lens.views loading string translations]
        Failure e -> do
            UI.div #+ [Lens.views locationPageError string translations]
        Data data'-> do
            select <- UI.div #. "select" #+ [element gradeSelect]
            inputSection <- UI.div #. "section" #+ [element gradeInput, element select]

            insertSection <- UI.div #. "section" #+ [element gradeInsert]

            UI.div #+ [element inputSection, element insertSection]


apMA :: Monad m => m (a -> m b) -> a -> m b
apMA f = join . ap f . pure

locationSection :: Env -> Window -> Translation -> Tabs -> Behavior Location.Model -> Behavior Grade.Model -> UI ()
locationSection env@Env{..} win translations tabs bLocationConfigFile bGrades = mdo
    let bModel = liftA2 mkModel bLocationConfigFile bGrades


    model <- currentValue bModel
    gradeInsert <- mkButton "insert" (Lens.view newGrade translations)

    gradeInput <- UI.input # set UI.id_ "focusGrade" # set UI.type_ "text" #. "input"
    bEditingInput <- bEditing gradeInput

    gradeSelect <- UI.select
    bEditingSelect <- bEditing gradeSelect

    case (unModel model) of
            Data data' -> void $ do
                gradeOptions <- mkGrades env (grades data')
                element gradeSelect # set children gradeOptions
                element gradeInput # set value (Grade.showGrade (grades data'))
            _ -> return ()

    let eNewGrade = filterJust $ mkNewGrade <$> grades <<$>> toJust <$> unModel <$> bModel <@ UI.click gradeInsert

    let inputB = inputGrade <$> grades <<$>> toJust <$> unModel <$> bModel
    let eValChange = filterJust $ flap <$> inputB <@> UI.valueChange gradeInput

    let selectB = selectGrade <$> grades <<$>> toJust <$> unModel <$> bModel
    let eSelectChange = filterJust $ apMA <$> selectB <@> filterJust (selectionChange' gradeSelect)

    let customEvent = head <$> unions' (eValChange :| [eNewGrade, eSelectChange])

    _ <- onEvent customEvent $ \e -> do
        liftIO $ Grade.writeGrades mGradesFile e


    liftIOLater $ onChange bModel $ \newModel -> runUI win $ do
        editingInput <- liftIO $ currentValue bEditingInput
        editingSelect <- liftIO $ currentValue bEditingSelect
        case (unModel newModel) of
                NotAsked -> void $ do
                    newContent <- mkLocation env win translations gradeSelect gradeInput gradeInsert newModel
                    element content # set children [newContent]

                Loading -> void $ do
                    unless (editingInput || editingSelect) $ void $ do
                        newContent <- mkLocation env win translations gradeSelect gradeInput gradeInsert newModel
                        element content # set children [newContent]

                Failure e -> void $ do
                    newContent <- mkLocation env win translations gradeSelect gradeInput gradeInsert newModel
                    element content # set children [newContent]

                Data data' -> void $ do
                    unless editingSelect $ void $ do
                        gradeOptions <- mkGrades env (grades data')
                        element gradeSelect # set children gradeOptions

                    unless editingInput $ void $ do
                        element gradeInput # set value (Grade.showGrade (grades data'))

                    unless (editingInput || editingSelect) $ void $ do
                        newContent <- mkLocation env win translations gradeSelect gradeInput gradeInsert newModel
                        element content # set children [newContent]


    val <- mkLocation env win translations gradeSelect gradeInput gradeInsert model
    content <- UI.div # set children [val]

    tabs' <- mkElement "nav" #. "section" #+ [mkTabs env tabs]
    navigation <- mkElement "footer" #. "section" #+ [mkNavigation env translations tabs]

    view <- UI.div #+ fmap element [ content ]

    void $ UI.getBody win # set children [tabs', view, navigation]

    UI.setFocus gradeInput -- Can only do this if element exists and should not do this if not focus





locationFileView :: Env -> Translation -> Location.LocationFile -> UI Element
locationFileView Env{..} translations locationFile = do
        title_ <- UI.div #+ [Lens.views locationTitle string translations]
        content <- UI.div #+ [UI.string (Location.unLocationFile locationFile)]

        pick <- mkFilePicker "locationFilePicker" (Lens.view pickLocation translations) $ \file ->
                when (file /= "") $
                        void $ Location.writeLocationFile mLocationConfigFile (Location.LocationFile file)

        make <- mkFileMaker "locationsPicker" (Lens.view newLocation translations) $ \file ->
                when (file /= "") $
                        void $ Location.writeLocationFile mLocationConfigFile (Location.LocationFile file)

        pickers <- UI.div #. "buttons has-addons" #+ [element pick, element make]

        open <- mkOpenFile "open" (Lens.view openLocation translations) (Location.unLocationFile locationFile)

        UI.div #. "section" # set children [ title_, content,  pickers, open]



inputGrade :: Grade.Grades -> String -> Grade.Grades
inputGrade grades name = Grade.Grades (ListZipper.mapFocus (const (Grade.Grade name)) (Grade.unGrades grades))


mkNewGrade :: Grade.Grades -> Grade.Grades
mkNewGrade grades = Grade.Grades $ ListZipper.insert (Grade.unGrades grades) (Grade.Grade "")


selectGrade :: Grade.Grades -> Int -> Maybe Grade.Grades
selectGrade grades selected =
        -- TODO this just wierd
        asum $ ListZipper.toNonEmpty $
                    ListZipper.iextend (\thisIndex grades' ->
                            if selected == thisIndex then
                                Just (Grade.Grades grades')
                            else
                                Nothing) (Grade.unGrades grades)



mkGrades :: Env -> Grade.Grades -> UI [Element]
mkGrades env grades' = do
    let currentGrade = Grade.extractGrade grades'
    let elems = ListZipper.iextend (\index grades'' ->
            let
                thisGrade = extract grades''
            in
                ( index
                , thisGrade == currentGrade
                , extract grades''
                )
            ) (Grade.unGrades grades')

    mapM (mkGrade env) (ListZipper.toList elems)


mkGrade :: Env -> (Int, Bool, Grade.Grade) -> UI Element
mkGrade Env{..} (thisIndex, isCenter, grade) = do
    let name = Grade.unGrade grade
    let option = UI.option # set value (show thisIndex) # set text name
    if isCenter then
        option # set UI.selected True
    else
        option
