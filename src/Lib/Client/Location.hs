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


import Lib.Client.Widgets.Entry

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


mkGrades :: Env -> Element -> Behavior Grades -> UI Element
mkGrades Env{..} input bGrades = mdo
    let bb = bGrades <&> unGrades <&> ListZipper.toList <&> (\xs -> fmap (\x -> UI.option # set text (unGrade x) # set value (unGrade x)) xs)
    let len = bGrades <&> unGrades <&> ListZipper.lefts <&> length <&> Just
    selector <- UI.select 
                    # sink items bb
                    # sink UI.selection len

    let e1 = UI.selectionChange selector

    let ebeh = fmap (\b n -> Grades (ListZipper.toN (unGrades b) n)) bGrades <@> filterJust e1

    onEvent ebeh $ \e -> do
        liftIO $ withMVar files $ \ Files{..} -> writeGrades gradesFile e
        UI.setFocus input

    return selector

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

locationSection :: Env -> Window -> Behavior LocationFile -> Grades -> Event Grades -> Tabs -> UI ()
locationSection env@Env{..} win bLocationFile grades eGrades tabs = mdo
    content <- locationFileView env bLocationFile

------------------------------------------------------------------------------
    eentry <- UI.input # set value (unGrade (extract (unGrades grades)))
                    # set (attr "id") "focusGrade"
                    # set UI.type_ "text"

    let e1 = UI.valueChange eentry
    be <- stepper grades eGrades
    let ebeh = fmap (\b i -> Grades (ListZipper.mapFocus (\y -> Grade i) (unGrades b))) be <@> e1

    bEditing <- stepper False $ and <$>
        unions [True <$ UI.focus eentry, False <$ UI.blur eentry]

    liftIO $ onChange be $ \s -> runUI win $ do
        editing <- liftIO $ currentValue bEditing
        unless editing $ void $ element eentry # set value (unGrade (extract (unGrades s)))

    onEvent ebeh $ \e ->
        liftIO $ withMVar files $ \ Files{..} -> writeGrades gradesFile e
------------------------------------------------------------------------------
    gradesContent <- gradesView env eentry bLocationFile be

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    view <- UI.div #+ fmap element [tabs', content, gradesContent, navigation]

    void $ UI.getBody win # set children [view]

    UI.setFocus eentry
