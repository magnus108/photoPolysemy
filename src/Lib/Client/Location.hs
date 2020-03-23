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


setSelectedGrade :: Grades -> String -> Grades
setSelectedGrade grades name = Grades (ListZipper.mapFocus (const (Grade name)) (unGrades grades))


locationSection :: Env -> Window -> Behavior LocationFile -> Event Grades -> Tabs -> UI ()
locationSection env@Env{..} win bLocationFile eGrades tabs = mdo
    -- INITIAL LOAD
    grades <- liftIO $ withMVar files $ \ Files{..} -> getGrades gradesFile

    content <- locationFileView env bLocationFile

    input <- UI.input # set value (showGrade grades)
                        # set (attr "id") "focusGrade"
                        # set UI.type_ "text"

    let eValChange = setSelectedGrade
                        <$> bGrades
                        <@> UI.valueChange input

    grades' <- mkGrades env grades

    selector <- UI.select # set children grades'

    let eSelChange = filterJust $ asum . ListZipper.toNonEmpty
            <$> ((\b pickedIndex -> ListZipper.iextend (\thisIndex grades'' ->
                if pickedIndex == show thisIndex
                   then Just (Grades grades'')
                   else Nothing
            ) (unGrades b) )
            <$> bGrades
            <@> selectionChange' selector)


    bGrades <- stepper grades $ head <$> unions'
        (eGrades :| [eValChange, eSelChange])

    gradesContent <- gradesView env bLocationFile bGrades

    bEditingInput <-  bEditing input
    bEditingSelector <- bEditing selector

    _ <- liftIOLater $ onChange bGrades $ \newGrades -> runUI win $ do
        editingSelector <- liftIO $ currentValue bEditingSelector
        unless editingSelector $ void $ do
            newGrades' <- mkGrades env newGrades
            element selector # set children newGrades'

        editingInput <- liftIO $ currentValue bEditingInput
        unless editingInput $ void $ element input # set value (showGrade newGrades)

    _ <- onEvent eValChange $ \e ->
        liftIO $ withMVar files $ \ Files{..} -> writeGrades gradesFile e

    _ <- onEvent eSelChange $ \e ->
        liftIO $ withMVar files $ \ Files{..} -> writeGrades gradesFile e

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    view <- UI.div #+ fmap element [tabs', content, input, selector, gradesContent, navigation]

    void $ UI.getBody win # set children [view]

    UI.setFocus input


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


gradesView :: Env -> Behavior LocationFile -> Behavior Grades -> UI Element
gradesView Env{..} _ bGrades = do
    gradeInsert <- mkButton "insert" "Tilføj ny"
    let eClick = bGrades <@ UI.click gradeInsert
    _ <- onEvent eClick $ \grades ->
        liftIO $ withMVar files $ \ Files{..} ->
            writeGrades gradesFile $ Grades $ ListZipper.insert (unGrades grades) (Grade "")

    UI.div #+ fmap element [gradeInsert]
