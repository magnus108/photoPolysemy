{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Location
    ( locationSection
    ) where


import qualified Relude.Unsafe as Unsafe
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


locationSection :: Env -> Window -> Behavior LocationFile -> Event Grades -> Tabs -> UI ()
locationSection env@Env{..} win bLocationFile eGrades tabs = mdo
-- INITIAL LOAD
    grades <- liftIO $ withMVar files $ \ Files{..} -> getGrades gradesFile

    content <- locationFileView env bLocationFile
------------------------------------------------------------------------------
    input <- UI.input # set value (unGrade (extract (unGrades grades)))
                    # set (attr "id") "focusGrade"
                    # set UI.type_ "text"

    selector <- UI.select
                    # sink items (bGrades <&> unGrades <&> ListZipper.toList <&> (\xs -> fmap (\x -> UI.option # set text (unGrade x) # set value (unGrade x)) xs))
                    # sink UI.selection (bGrades <&> unGrades <&> ListZipper.lefts <&> length <&> Just)

    let eChange = eGrades
    let eValChange = UI.valueChange input
    let eSelChange = UI.selectionChange selector

    let lulwut = (\b i -> Grades (ListZipper.mapFocus (\y -> Grade i) (unGrades b))) <$> bGrades <@> eValChange
    let lulwut2 = (\b n -> Grades (ListZipper.toN (unGrades b) n)) <$> bGrades <@> filterJust eSelChange

    bGrades <- stepper grades $ Unsafe.head <$> unions
        [ eGrades
        , lulwut
        , lulwut2
        ]

    --HELPER
    bEditing <- stepper False $ and <$>
        unions [True <$ UI.focus input, False <$ UI.blur input]

    -- OPTIMIZATION THING
    liftIOLater $ onChange bGrades $ \s -> runUI win $ do
        editing <- liftIO $ currentValue bEditing
        unless editing $ void $ element input # set value (unGrade (extract (unGrades s)))

    onEvent lulwut $ \e ->
        liftIO $ withMVar files $ \ Files{..} -> writeGrades gradesFile e

    onEvent lulwut2 $ \e -> do
        liftIO $ withMVar files $ \ Files{..} -> writeGrades gradesFile e
        UI.setFocus input


------------------------------------------------------------------------------
--    gradesContent <- gradesView env eentry bLocationFile be

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    view <- UI.div #+ fmap element [tabs', content, input, selector, navigation]

    void $ UI.getBody win # set children [view]

    UI.setFocus input
























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
