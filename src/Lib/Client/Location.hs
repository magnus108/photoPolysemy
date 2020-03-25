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


setSelectedGrade :: Grades -> String -> Grades
setSelectedGrade grades name = Grades (ListZipper.mapFocus (const (Grade name)) (unGrades grades))


locationSection :: Env -> Window -> Event (Either String LocationFile) -> Event Grades -> Tabs -> UI ()
locationSection env@Env{..} win eLocationConfigFile eGrades tabs = mdo
    -- INITIAL LOAD
    grades <- liftIO $ withMVar files $ \ Files{..} -> getGrades gradesFile
    locationFile <- liftIO $ withMVar files $ \ Files{..} -> getLocationFile locationConfigFile

    bLocationFile <- stepper locationFile eLocationConfigFile

    content <- UI.div # sink item (locationFileView env <$> bLocationFile)

    gradeInsert <- mkButton "insert" "Tilføj ny"

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


    let eClick = bGrades <@ UI.click gradeInsert

    _ <- onEvent eClick $ \grades'' ->
        liftIO $ withMVar files $ \ Files{..} ->
            writeGrades gradesFile $ Grades $ ListZipper.insert (unGrades grades'') (Grade "")


    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    view <- UI.div #+ fmap element [tabs', content, input, selector, gradeInsert, navigation]

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

