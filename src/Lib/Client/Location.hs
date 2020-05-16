{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Location
    ( locationSection
    ) where


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



mkLocation :: Env -> Translation -> Location.Model -> Grade.Model -> UI Element
mkLocation env@Env{..} translations location grades =
    case Location.unModel location of
        NotAsked -> UI.p #+ [Lens.views starting string translations]
        Loading -> UI.p #+ [Lens.views loading string translations]
        Failure e -> do
            UI.div #+ [string "ok"]
        Data _ -> do
            UI.div #+ [string "lol"]

locationSection :: Env -> Window -> Translation -> Tabs -> Behavior Location.Model -> Behavior Grade.Model -> UI ()
locationSection env@Env{..} win translation tabs bLocationConfigFile bGrades = do
    let bView = mkLocation env translation <$> bLocationConfigFile <*> bGrades
    content <- UI.div #. "section" # sink item bView

    tabs' <- mkElement "nav" #. "section" #+ [mkTabs env tabs]
    navigation <- mkElement "footer" #. "section" #+ [mkNavigation env translation tabs]

    view <- UI.div #+ fmap element [ content ]

    void $ UI.getBody win # set children [tabs', view, navigation]
    return ()





        {-
    (eInitial, eInitialHandle) <- liftIO newEvent
    let eSplit = splitData eGrades

    bModel <- accumB initialState $ concatenate' <$> unions'
        ((Lens.set grades . Data <$> (success eSplit))
            :| [ Lens.set grades . Failure <$> failure eSplit
               , Lens.set grades <$> eInitial
               , Lens.set grades Loading <$ (lloading eSplit)
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
                    text' <- string "Starting.."
                    void $ element content # set children [text']
            Loading -> do
                unless (editingInput || editingSelector) $ void $ do
                    text' <- string "Loading.."
                    void $ element content # set children [text']
            Failure _ -> do
                unless (editingInput || editingSelector) $ void $ do
                    para <- UI.p # set text "Noget fejl klasser"
                    void $ element content # set children [para]
            Data grades' -> do
                unless (editingSelector) $ void $ do
                    grades'' <- mkGrades env grades'
                    element selector # set children grades''
                unless (editingInput) $ void $ do
                    element input # set value (showGrade grades')
                unless (editingInput || editingSelector) $ void $ do
                    void $ element content # set children [gradeInsert, input, selector]

    let customEvent = head <$> unions' (eNewGrade :| [eValChange, eSelChange])

    _ <- onEvent customEvent $ \e-> do
        liftIO $ withMVar mGradesFile $ \file -> do
            liftIO $ writeGrades file e

    _ <- getGrades mGradesFile eInitialHandle

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env translation tabs

    view <- UI.div #+ fmap element
        [ tabs'
        , content
        , locationContent
        , navigation
        ]

    void $ UI.getBody win # set children [view]

    UI.setFocus input -- Can only do this if element exists and should not do this if not focus
    -}


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


inputGrade :: Grade.Model -> String -> Maybe Grade.Grades
inputGrade model name =
    case Grade._grades model of
        Data grades' -> Just $ Grade.Grades (ListZipper.mapFocus (const (Grade.Grade name)) (Grade.unGrades grades'))
        _ -> Nothing

newGrade :: Grade.Model -> Maybe Grade.Grades
newGrade model =
    case Grade._grades model of
        Data grades' -> Just $ Grade.Grades $ ListZipper.insert (Grade.unGrades grades') (Grade.Grade "")
        _ -> Nothing

selectGrade :: Grade.Model -> Int -> Maybe Grade.Grades
selectGrade model selected =
    case Grade._grades model of
        -- TODO this just wierd
        Data grades' -> asum $ ListZipper.toNonEmpty $
                    ListZipper.iextend (\thisIndex grades'' ->
                            if selected == thisIndex then
                                Just (Grade.Grades grades'')
                            else
                                Nothing) (Grade.unGrades grades')
        _ -> Nothing



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

    grades'' <- mapM (mkGrade env) elems

    return (ListZipper.toList grades'')


mkGrade :: Env -> (Int, Bool, Grade.Grade) -> UI Element
mkGrade Env{..} (thisIndex, isCenter, grade) = do
    let name = Grade.unGrade grade
    let option = UI.option # set value (show thisIndex) # set text name
    if isCenter then
        option # set UI.selected True
    else
        option
        -}
