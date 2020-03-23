{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Location
    ( locationSection
    ) where


import Lib.Client.Utils

import Reactive.Threepenny
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Utils.Comonad
import qualified Utils.ListZipper as ListZipper

import Lib.Grade
import Lib.Tab
import Lib.Location
import Lib.Client.Tab

import Lib.Client.Element
import Lib.App (Env(..), Files(..))
import Control.Concurrent.MVar (withMVar)



locationFileView :: Env -> Behavior LocationFile -> UI Element
locationFileView Env{..} bLocationFile = do
    let title_ = UI.div #+ [UI.string "Lokation"]
    let content = bLocationFile <&> \locationFile -> UI.div #+ [UI.string (unLocationFile locationFile)]

    pick <- mkFilePicker "locationFilePicker" "Vælg lokations" $ \file ->
            when (file /= "") $
                withMVar files $ \ Files{..} ->
                    writeLocationFile locationConfigFile (LocationFile file)

    make <- mkFileMaker "locationsPicker" "Ny CSV" $ \file ->
            when (file /= "") $
                withMVar files $ \ Files{..} ->
                    writeLocationFile locationConfigFile (LocationFile file)

    let pickers = UI.div #. "buttons has-addons" #+ [element pick, element make]

    let open = bLocationFile <&> mkOpenFile "open" "Åben csv" . unLocationFile

    UI.div # sink items (sequenceA [pure title_, content, pure pickers, open])


locationSection :: Env -> Window -> Behavior LocationFile -> Event Grades -> Tabs -> UI ()
locationSection env@Env{..} win bLocationFile eGrades tabs = mdo
-- INITIAL LOAD
    grades <- liftIO $ withMVar files $ \ Files{..} -> getGrades gradesFile

    content <- locationFileView env bLocationFile
------------------------------------------------------------------------------
    input <- UI.input # set value (showGrade grades)
                        # set (attr "id") "focusGrade"
                        # set UI.type_ "text"

----------------------------------------------------------------------------------------
    let currentGrade = extractGrade grades
    let elems = ListZipper.iextend (\index grades'' ->
            let
                thisGrade = extract grades''
            in
                ( index
                , thisGrade == currentGrade
                , selector
                , extract grades''
                , Grades grades''
                )
            ) (unGrades grades)

    let grades' = ListZipper.toNonEmpty $ fmap (mkGrade env) elems
    childs <- mapM fst grades'
    let eSelChange = fmap (filterJust . snd) grades'
    selector <- UI.select # set children (toList childs)

----------------------------------------------------------------------------------------
    let eValChange = (\b i -> Grades (ListZipper.mapFocus (const (Grade i)) (unGrades b))) <$> bGrades <@> UI.valueChange input


    bGrades <- stepper grades $ head <$> unions'
        (ListZipper.appendr [eGrades ,eValChange] eSelChange)


    --HELPER
    let bEditing element' = stepper False $ and <$> unions [True <$ UI.focus element', False <$ UI.blur element']

    bEditingInput <-  bEditing input
    --bEditingSelector <- bEditing selector


    -- OPTIMIZATION THING
    _ <- liftIOLater $ onChange bGrades $ \s -> runUI win $ do
                        --    opts' <- sequence $ fmap (\x -> UI.option # set text (unGrade x) # set value (unGrade x)) $ ListZipper.toList (unGrades s)
                        --   let len' = Just (length (ListZipper.lefts (unGrades s)))
                        --  _ <- element selector # set children opts' # set UI.selection len'
        --editingSelector <- liftIO $ currentValue bEditingSelector
        --unless editingSelector $ void $ 
        --    element input # set value (unGrade (extractGrade s))

        editingInput <- liftIO $ currentValue bEditingInput
        unless editingInput $ void $ element input # set value (unGrade (extractGrade s))

    _ <- onEvent eValChange $ \e ->
        liftIO $ withMVar files $ \ Files{..} -> writeGrades gradesFile e

    _ <- onEvent (head <$> unions' eSelChange) $ \e -> do
        liftIO $ withMVar files $ \ Files{..} -> writeGrades gradesFile e

------------------------------------------------------------------------------

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    view <- UI.div #+ fmap element [tabs', content, input, selector, navigation]

    void $ UI.getBody win # set children [view]

    UI.setFocus input



mkGrade :: Env -> (Int, Bool, Element, Grade, Grades) -> (UI Element, Event (Maybe Grades))
mkGrade Env{..} (thisIndex, isCenter, selector, grade, grades) = do
    let name = unGrade grade

    let option = UI.option # set value (show thisIndex) # set text name

    let event = selectionChange' selector <&> \pickedIndex -> if pickedIndex == show thisIndex then Just grades else Nothing

    if isCenter then
        (option # set UI.selected True, event)
    else
        (option, event)





















    {-



mkGrades :: Env -> Element -> Behavior Grades -> UI Element
mkGrades Env{..} input bGrades = mdo

    {-
    let elements' = bGrades <&> (\grades -> do
            let currentGrade = extract (unGrades grades)
            let elems = ListZipper.iextend (\index grades'' ->
                    let
                        thisGrade = extract grades''
                    in
                        ( index, thisGrade == currentGrade
                        , selector
                        , extract grades''
                        , Grades grades''
                        )
                    ) (unGrades grades)

            let grades' = fmap (mkGrade env) elems
            ListZipper.toList grades'
            )

    element selector # sink items elements'
    -}


mkGrade :: Env -> (Int, Bool, Element, Grade, Grades) -> UI Element
mkGrade Env{..} (thisIndex, isCenter, selector, grade, grades) = do
    UI.on UI.selectionChange selector $ \pickedIndex ->
        when (fromMaybe (-1) pickedIndex == thisIndex) $
            liftIO $ withMVar files $ \ Files{..} -> do
                writeGrades gradesFile grades

    let name = show grade

    let option = UI.option # set (attr "value") name  # set text name

    if isCenter then
        option # set (UI.attr "selected") ""
    else
        option
        

gradesView :: Env -> Element -> Behavior LocationFile -> Behavior Grades -> UI Element
gradesView env@Env{..} input _ bGrades = do
    gradeInsert <- mkButton "insert" "Tilføj ny"
    UI.on UI.click gradeInsert $ \_ -> do
        liftIO $ withMVar files $ \ Files{..} -> do
            --TODO fix this up
            grades <- currentValue bGrades
            writeGrades gradesFile $ Grades $ ListZipper.insert (unGrades grades) (Grade "")
        UI.setFocus input


    view <- mkGrades env input bGrades

    select <- UI.div #+ fmap element [view, input]
    UI.div #+ fmap element [gradeInsert, select]
-}
