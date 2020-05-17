{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Location
    ( locationSection
    ) where


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



locationSection :: Env -> Window -> Translation -> Tabs -> Behavior Location.Model -> Behavior Grade.Model -> UI ()
locationSection env@Env{..} win translations tabs bLocationConfigFile bGrades = mdo
--Items

    gradeInsert <- mkButton "insert" "Tilføj ny"

    let selectorItems = fromMaybe [] <$> (mkGrades env . grades <<$>> toJust <$> unModel <$> bModel)
    selector <- UI.select # sink items selectorItems

    input <- UI.input # set UI.id_ "focusGrade" # set UI.type_ "text"

    let eNewGrade = newGrade <$> filterJust (grades <<$>> toJust <$> unModel <$> bModel <@ UI.click gradeInsert)
    let eValChange = filterJust $ (\m i -> fmap (inputGrade i . grades) m) <$> toJust <$> unModel <$> bModel <@> UI.valueChange input
    let eSelChange = filterJust $ (\m i -> m >>= (selectGrade i . grades)) <$> toJust <$> unModel <$> bModel <@> (filterJust (selectionChange' selector))

        {-
    bEditingSelector <- bEditing selector
    bEditingInput <- bEditing input

    liftIOLater $ onChange bModel $ \newModel -> runUI win $ do
        editingInput <- liftIO $ currentValue bEditingInput
        editingSelector <- liftIO $ currentValue bEditingSelector

        case unModel newModel of
            NotAsked -> do
                unless (editingInput || editingSelector) $ void $ do
                    text' <- Lens.views starting string translations
                    void $ element content # set children [text']
            Loading -> do
                unless (editingInput || editingSelector) $ void $ do
                    text' <- Lens.views loading string translations
                    void $ element content # set children [text']
            Failure _ -> do
                unless (editingInput || editingSelector) $ void $ do
                    text' <- Lens.views locationPageError string translations
                    void $ element content # set children [text']
            Data data' -> do
                unless (editingSelector) $ void $ do
                    grades'' <- mkGrades env (grades data')
                    element selector # set children grades''
                unless (editingInput) $ void $ do
                    element input # set value (Grade.showGrade (grades data'))
                unless (editingInput || editingSelector) $ void $ do
                    void $ element content # set children [gradeInsert, input, selector]


    let customEvent = head <$> unions' (eNewGrade :| [eValChange, eSelChange])

    _ <- onEvent customEvent $ \e -> do
        liftIO $ Grade.writeGrades mGradesFile e
        -}

    let bModel = liftA2 mkModel bLocationConfigFile bGrades

    let mkLocation model =
            case unModel model of
                NotAsked ->
                    [Lens.views starting string translations]
                Loading ->
                    [Lens.views loading string translations]
                Failure e -> do
                    [Lens.views locationPageError string translations]
                Data _ -> do
                    fmap element [gradeInsert, input, selector]

    let bView = mkLocation <$> bModel

    content <- UI.div #. "section" # sink items bView

    tabs' <- mkElement "nav" #. "section" #+ [mkTabs env tabs]
    navigation <- mkElement "footer" #. "section" #+ [mkNavigation env translations tabs]

    view <- UI.div #+ fmap element [ content ]

    void $ UI.getBody win # set children [tabs', view, navigation]

    --UI.setFocus input -- Can only do this if element exists and should not do this if not focus




    {-
locationFileView :: Env -> Either String Location.LocationFile -> UI Element
locationFileView Env{..} = \case
    Left _ -> UI.div #+ [UI.string "FEJL"]
    Right locationFile -> do
        title_ <- UI.div #+ [UI.string "Lokation"]
        content <- UI.div #+ [UI.string (Location.unLocationFile mLocationFile)]

        pick <- mkFilePicker "locationFilePicker" "Vælg lokations" $ \file ->
                when (file /= "") $
                    withMVar files $ \ Files{..} ->
                        Location.writeLocationFile mLocationConfigFile (Location.LocationFile file)

        make <- mkFileMaker "locationsPicker" "Ny CSV" $ \file ->
                when (file /= "") $
                    withMVar files $ \ Files{..} ->
                        Location.writeLocationFile mLocationConfigFile (Location.LocationFile file)

        pickers <- UI.div #. "buttons has-addons" #+ [element pick, element make]

        open <- mkOpenFile "open" "Åben csv" (Location.unLocationFile locationFile)

        UI.div # set children [ title_, content,  pickers, open]

        -}


inputGrade :: String -> Grade.Grades -> Grade.Grades
inputGrade name grades = Grade.Grades (ListZipper.mapFocus (const (Grade.Grade name)) (Grade.unGrades grades))


newGrade :: Grade.Grades -> Grade.Grades
newGrade grades = Grade.Grades $ ListZipper.insert (Grade.unGrades grades) (Grade.Grade "")


selectGrade :: Int -> Grade.Grades -> Maybe Grade.Grades
selectGrade selected grades =
        -- TODO this just wierd
        asum $ ListZipper.toNonEmpty $
                    ListZipper.iextend (\thisIndex grades' ->
                            if selected == thisIndex then
                                Just (Grade.Grades grades')
                            else
                                Nothing) (Grade.unGrades grades)



mkGrades :: Env -> Grade.Grades -> [UI Element]
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

    fmap (mkGrade env) (ListZipper.toList elems)


mkGrade :: Env -> (Int, Bool, Grade.Grade) -> UI Element
mkGrade Env{..} (thisIndex, isCenter, grade) = do
    let name = Grade.unGrade grade
    let option = UI.option # set value (show thisIndex) # set text name
    if isCenter then
        option # set UI.selected True
    else
        option
