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



    -- kun fordi ikke noget initial event.
mkLocation
    :: Env
    -> Window
    -> Translation
    -> Element
    -> Element
    -> Element
    -> Model
    -> UI Element
mkLocation env@Env {..} win translations gradeSelect gradeInput gradeInsert model
    = do
        case unModel model of
            NotAsked  -> UI.div #+ [Lens.views starting string translations]
            Loading   -> UI.div #+ [Lens.views loading string translations]
            Failure e -> do
                UI.div #+ [Lens.views locationPageError string translations]
            Data data' -> do
                locationFileSection <- locationFileView env
                                                        translations
                                                        (location data')
                select  <- UI.div #. "select" #+ [element gradeSelect]

                content <-
                    UI.div
                    #. "section"
                    #+ [ UI.div
                         #. "field is-horizontal"
                         #+ [ UI.div
                              #. "field-body"
                              #+ [ UI.div
                                 #. "field"
                                 #+ [UI.p #. "control" #+ [element gradeInput]]
                                 , UI.div
                                 #. "field"
                                 #+ [UI.p #. "control" #+ [element select]]
                                 ]
                            ]
                       ]

                insertSection <- UI.div #. "section" #+ [element gradeInsert]

                UI.div
                    #+ [ element locationFileSection
                       , element content
                       , element insertSection
                       ]

gradeItem
    :: Env
    -> Behavior (Maybe Grade.Grades)
    -> UI ((Element, Element), Tidings (Maybe Grade.Grades)) --Tidings Grade.Grades)
gradeItem env bItem = do
    let asString = maybe "" Grade.showGrade <$> bItem
    let asList   = maybe [] (mkGrades env) <$> bItem

    input <- UI.entry asString
    select <- UI.select # sink items asList

    let tInput = inputGrade <$> UI.userText input
        bInput = facts tInput -- asString såå jeg skal også bruge aslist
        eInput = rumors tInput

    let eSelect = selectGrade <$> filterJust (selectionChange' select)

    let e = concatenate' <$> unions' (eSelect :| [eInput])

    let b = liftA2 (<$>) bInput bItem
    let gg = fmap (<&>) b <@> e
    let superTide = tidings b gg

    return ((getElement input, getElement select), superTide)


locationSection
    :: Env -> Window -> Translation -> Tabs -> Behavior Model -> UI ()
locationSection env@Env {..} win translations tabs bModel = mdo

    ((input, select), tGradeItem) <- gradeItem env bGrade

    let bItem :: Behavior (Maybe Item)
        bItem = toJust <$> unModel <$> bModel

        bGrade :: Behavior (Maybe Grade.Grades)
        bGrade = grades <<$>> bItem


    let eGradeItemIn = rumors $ tGradeItem

    _ <- onEvent eGradeItemIn $ \e -> do
        case e of
            Nothing -> return ()
            Just i  -> void $ liftIO $ Grade.writeGrades mGradesFile i



    {-
        gradeInput  <- entry bGradeInput
        bGradeInput <- stepper "" . rumors $ userText gradeInput

        let tGrade = userText gradeInput
            bGrade = facts  tGrade
            eGrade = rumors tGrade


        let inputB = inputGrade <$> grades <<$>> toJust <$> unModel <$> bModel
        let what = inputB <??> bGrade
        let what2 = Grade.showGrade <<$>> what
        let what3 = fromMaybe "" <$> what2

        let eValChange = filterJust $ flap <$> inputB <@> (rumors (userText gradeInput))



        model       <- currentValue bModel
        gradeSelect    <- UI.select
        bEditingSelect <- bEditing gradeSelect

        case (unModel model) of
            Data data' -> void $ do
                gradeOptions <- mkGrades env (grades data')
                element gradeSelect # set children gradeOptions
                --getElement gradeInput # set value (Grade.showGrade (grades data'))
            _ -> return ()

        let eNewGrade =
                filterJust
                    $     mkNewGrade
                    <$>   grades
                    <<$>> toJust
                    <$>   unModel
                    <$>   bModel
                    <@    UI.click gradeInsert


        let selectB =
                selectGrade <$> grades <<$>> toJust <$> unModel <$> bModel
        let eSelectChange = filterJust $ apMA <$> selectB <@> filterJust
                (selectionChange' gradeSelect)

        gradeInsert <- mkButton "insert" (Lens.view newGrade translations)
        let customEvent =
                head <$> unions' (eNewGrade :| [eSelectChange])

        _ <- onEvent customEvent $ \e -> do
            liftIO $ Grade.writeGrades mGradesFile e


        liftIOLater $ onChange bModel $ \newModel -> runUI win $ do
            --editingInput  <- liftIO $ currentValue bEditingInput
            editingSelect <- liftIO $ currentValue bEditingSelect
            case (unModel newModel) of
                NotAsked -> void $ do
                    newContent <- mkLocation env
                                             win
                                             translations
                                             gradeSelect
                                             (getElement gradeInput)
                                             gradeInsert
                                             newModel
                    element content # set children [newContent]

                Loading -> void $ do
                    unless ( editingSelect) $ void $ do
                        newContent <- mkLocation env
                                                 win
                                                 translations
                                                 gradeSelect
                                                 (getElement gradeInput)
                                                 gradeInsert
                                                 newModel
                        element content # set children [newContent]

                Failure e -> void $ do
                    newContent <- mkLocation env
                                             win
                                             translations
                                             gradeSelect
                                             (getElement gradeInput)
                                             gradeInsert
                                             newModel
                    element content # set children [newContent]

                Data data' -> void $ do
                    unless editingSelect $ void $ do
                        gradeOptions <- mkGrades env (grades data')
                        element gradeSelect # set children gradeOptions

    {-
                    unless editingInput $ void $ do
                        element gradeInput
                            # set value (Grade.showGrade (grades data'))
-}

                    unless (editingSelect) $ void $ do
                        newContent <- mkLocation env
                                                 win
                                                 translations
                                                 gradeSelect
                                                 (getElement gradeInput)
                                                 gradeInsert
                                                 newModel
                        element content # set children [newContent]


        val <- mkLocation env
                          win
                          translations
                          gradeSelect
                          (getElement gradeInput)
                          gradeInsert
                          model
        content    <- UI.div # set children [val]
        -}


    let bView = bModel <&> (\x -> case unModel x of
            NotAsked  -> UI.div #+ [Lens.views starting string translations]
            Loading   -> UI.div #+ [Lens.views loading string translations]
            Failure e -> do
                UI.div #+ [Lens.views locationPageError string translations]
            Data data' -> do
                UI.div #+ [element input, element select])

    content <- UI.div # sink item bView


    tabs'      <- mkElement "nav" #. "section" #+ [mkTabs env tabs]
    navigation <-
        mkElement "footer" #. "section" #+ [mkNavigation env translations tabs]

    view <- UI.div #+ fmap element [content]

    void $ UI.getBody win # set children [tabs', view, navigation]

        --UI.setFocus (getElement gradeInput) -- Can only do this if element exists and should not do this if not focus





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
mkGrades env grades' = do
    let currentGrade = Grade.extractGrade grades'
    let elems = ListZipper.iextend
            (\index grades'' ->
                let thisGrade = extract grades''
                in  (index, thisGrade == currentGrade, extract grades'')
            )
            (Grade.unGrades grades')

    map (mkGrade env) (ListZipper.toList elems)



mkGrade :: Env -> (Int, Bool, Grade.Grade) -> UI Element
mkGrade Env {..} (thisIndex, isCenter, grade) = do
    let name   = Grade.unGrade grade
    let option = UI.option # set value (show thisIndex) # set text name
    if isCenter then option # set UI.selected True else option
