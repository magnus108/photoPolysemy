{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Location
    ( locationSection
    ) where


import Control.Concurrent
import Lib.Client.Utils
import qualified Control.Lens as Lens

import Reactive.Threepenny
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Utils.Comonad
import qualified Utils.ListZipper as ListZipper

import Lib.Data
import Lib.Grade
import Lib.Tab
import Lib.Location
import Lib.Client.Tab

import Lib.Client.Element
import Lib.App (Env(..), Files(..))
import Control.Concurrent.MVar (withMVar)



locationFileView :: Env -> Either String LocationFile -> UI Element
locationFileView Env{..} = \case
    Left _ -> UI.div #+ [UI.string "FEJL"]
    Right locationFile -> do
        title_ <- UI.div #+ [UI.string "Lokation"]
        content <- UI.div #+ [UI.string (unLocationFile locationFile)]

        pick <- mkFilePicker "locationFilePicker" "Vælg lokations" $ \file ->
                when (file /= "") $
                    withMVar files $ \ Files{..} ->
                        writeLocationFile locationConfigFile (LocationFile file)

        make <- mkFileMaker "locationsPicker" "Ny CSV" $ \file ->
                when (file /= "") $
                    withMVar files $ \ Files{..} ->
                        writeLocationFile locationConfigFile (LocationFile file)

        pickers <- UI.div #. "buttons has-addons" #+ [element pick, element make]

        open <- mkOpenFile "open" "Åben csv" (unLocationFile locationFile)

        UI.div # set children [ title_, content,  pickers, open]


inputGrade :: Model -> String -> Maybe Grades
inputGrade model name =
    case _grades model of
        Data grades -> Just $ Grades (ListZipper.mapFocus (const (Grade name)) (unGrades grades))
        _ -> Nothing

newGrade :: Model -> Maybe Grades
newGrade model =
    case _grades model of
        Data grades -> Just $ Grades $ ListZipper.insert (unGrades grades) (Grade "")
        _ -> Nothing

selectGrade :: Model -> Int -> Maybe Grades
selectGrade model selected =
    case _grades model of
        -- TODO this just wierd
        Data grades -> asum $ ListZipper.toNonEmpty $
                    ListZipper.iextend (\thisIndex grades' ->
                            if selected == thisIndex then
                                Just (Grades grades')
                            else
                                Nothing) (unGrades grades)
        _ -> Nothing

locationSection :: Env -> Window -> Event (Either String LocationFile) -> Event (Data String Grades) -> Tabs -> UI ()
locationSection env@Env{..} win eLocationConfigFile eGrades tabs = do
    (eInitial, eInitialHandle) <- liftIO newEvent
    let (_, eLoading, eGradesErr, eGradesSucc) = splitData eGrades

    bModel <- accumB initialState $ concatenate' <$> unions'
        ((Lens.set grades . Data <$> eGradesSucc)
            :| [ Lens.set grades . Failure <$> eGradesErr
               , Lens.set grades <$> eInitial
               , Lens.set grades Loading <$ eLoading
               ])


    locationFile <- liftIO $ withMVar files $ \ Files{..} -> getLocationFile locationConfigFile
    bLocationFile <- stepper locationFile eLocationConfigFile
    locationContent <- UI.div # sink item (locationFileView env <$> bLocationFile)


    content <- UI.div --TODO initialize

    gradeInsert <- mkButton "insert" "Tilføj ny"
    input <- UI.input # set UI.id_ "focusGrade" # set UI.type_ "text"
    selector <- UI.select

    let eNewGrade = filterJust $ newGrade <$> bModel <@ UI.click gradeInsert
    let eValChange = filterJust $ inputGrade <$> bModel <@> UI.valueChange input
    let eSelChange = filterJust $ selectGrade <$> bModel <@> (filterJust (selectionChange' selector))


    bEditingSelector <- bEditing selector
    bEditingInput <- bEditing input

    _ <- onChanges bModel $ \newModel -> do
        editingInput <- liftIO $ currentValue bEditingInput
        editingSelector <- liftIO $ currentValue bEditingSelector

        case _grades newModel of
            NotAsked -> do
                unless (editingInput || editingSelector) $ void $ do
                    item <- string "Starting.."
                    void $ element content # set children [item]
            Loading -> do
                unless (editingInput || editingSelector) $ void $ do
                    item <- string "Loading.."
                    void $ element content # set children [item]
            Failure _ -> do
                unless (editingInput || editingSelector) $ void $ do
                    para <- UI.p # set text "Noget fejl klasser"
                    void $ element content # set children [para]
            Data grades -> do
                unless (editingSelector) $ void $ do
                    grades' <- mkGrades env grades
                    element selector # set children grades'
                unless (editingInput) $ void $ do
                    element input # set value (showGrade grades)
                unless (editingInput || editingSelector) $ void $ do
                    void $ element content # set children [gradeInsert, input, selector]

    let customEvent = head <$> unions' (eNewGrade :| [eValChange, eSelChange])

    _ <- onEvent customEvent $ \e-> do
        liftIO $ withMVar mGradesFile $ \file -> do
            liftIO $ writeGrades file e

    _ <- getGrades mGradesFile eInitialHandle

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    view <- UI.div #+ fmap element
        [ tabs'
        , content
        , locationContent
        , navigation
        ]

    void $ UI.getBody win # set children [view]

    UI.setFocus input -- Can only do this if element exists and should not do this if not focus



mkGrades :: Env -> Grades -> UI [Element]
mkGrades env grades = do
    let currentGrade = extractGrade grades
    let elems = ListZipper.iextend (\index grades' ->
            let
                thisGrade = extract grades'
            in
                ( index
                , thisGrade == currentGrade
                , extract grades'
                )
            ) (unGrades grades)

    grades' <- mapM (mkGrade env) elems

    return (ListZipper.toList grades')


mkGrade :: Env -> (Int, Bool, Grade) -> UI Element
mkGrade Env{..} (thisIndex, isCenter, grade) = do
    let name = unGrade grade
    let option = UI.option # set value (show thisIndex) # set text name
    if isCenter then
        option # set UI.selected True
    else
        option
