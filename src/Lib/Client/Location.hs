module Lib.Client.Location
    ( locationSection
    ) where

import Lib.Client.Utils
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


mkGrades :: Env -> Behavior Grades -> UI Element
mkGrades env bGrades = do
    selector <- UI.select

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


mkGrade :: Env -> (Int, Bool, Element, Grade, Grades) -> UI Element
mkGrade Env{..} (thisIndex, isCenter, selector, grade, grades) = do
    UI.on UI.selectionChange selector $ \pickedIndex ->
        when (fromMaybe (-1) pickedIndex == thisIndex) $
            liftIO $ withMVar files $ \ Files{..} ->
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
            _ <- element input # set value ""
            UI.setFocus input


    view <- mkGrades env bGrades

    UI.on UI.keyup input $ const $ do
            val <- UI.get value input
            liftIO $ withMVar files $ \ Files{..} -> do
                    grades <- currentValue bGrades
                    writeGrades gradesFile $
                        Grades $ ListZipper.mapFocus (\_ -> Grade val) (unGrades grades)


    UI.div #+ fmap element [gradeInsert, view, input]

locationSection :: Env -> Window -> Behavior LocationFile -> Behavior Grades -> Tabs -> UI ()
locationSection env@Env{..} win bLocationFile bGrades tabs = do
    content <- locationFileView env bLocationFile

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    gradeName <- UI.input #. "input" # set (attr "id") "focusGrade" # set UI.type_ "text"
    gradesContent <- gradesView env gradeName bLocationFile bGrades


    view <- UI.div #+ fmap element
        [tabs', content, gradesContent, navigation]

    void $ UI.getBody win #+ fmap element [view]

    grades <- currentValue bGrades
    _ <- element gradeName # set value (unGrade (extract (unGrades grades)))
    UI.setFocus gradeName
