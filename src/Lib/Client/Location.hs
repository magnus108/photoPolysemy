{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Location
    ( locationSection
    , mkGrades
    , mkModel
    , selectGrade
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


gradeItem
    :: Env
    -> Window
    -> Translation
    -> Behavior Model
    -> UI ((Element, Element, Element), Tidings Model)
gradeItem env win translations bModel = do
    let bItem   = toJust <$> unModel <$> bModel
        bGrades = grades <<$>> bItem

    val <- currentValue bGrades
    input          <- UI.input # set value (maybe "" Grade.showGrade val)
    bEditingInput                     <- bEditing input
    liftIOLater $ onChange bGrades $ \grades' -> runUI win $ do
        editingInput  <- liftIO $ currentValue bEditingInput
        when (not editingInput) $ void $ do
            let string = maybe "" Grade.showGrade grades'
            element input # set value string


    let bOptions = maybe [] (mkGrades env) <$> bGrades
    select         <- UI.select # sink items bOptions

    button <- mkButton "insert" (Lens.view newGrade translations)

    let eClick    = mkNewGrade <$ UI.click button
    let eInput    = inputGrade <$> UI.valueChange input
    let eSelect   = selectGrade <$> filterJust (selectionChange' select)

    let allEvents = concatenate' <$> unions' (eSelect :| [eInput, eClick])


    let
        superTide =
            tidings bModel
                $   filterJust
                $   fmap
                        (\m f -> case toJust (unModel m) of
                            Nothing -> Nothing
                            Just x ->
                                Just
                                    (Model
                                        (Data (Item (location x) (f (grades x))))
                                    )
                        )
                        bModel
                <@> allEvents


    return ((getElement input, getElement select, getElement button), superTide)


locationSection
    :: Env -> Window -> Translation -> Tabs -> Behavior Model -> UI ()
locationSection env@Env {..} win translations tabs bModel = mdo
    ((input, select, button), tModel) <- gradeItem env win translations bModel


    select' <- UI.div #. "select" #+ [element select]
    content <-
        UI.div
        #. "section"
        #+ [ UI.div
                #. "field is-horizontal"
                #+ [ UI.div
                    #. "field-body"
                    #+ [ UI.div
                        #. "field"
                        #+ [ UI.p
                            #. "control"
                            #+ [element input #. "input"]
                        ]
                        , UI.div
                        #. "field"
                        #+ [UI.p #. "control" #+ [element select']]
                        ]
                ]
            ]


    let bItem   = toJust <$> unModel <$> bModel
    
    let bLocationFileView = fmap concat $ locationFileView env translations . location <<$>> bItem

    locationFileSection <- UI.div #. "section" # sink items bLocationFileView
                                            
    insertSection <- UI.div #. "section" #+ [element button]

    let bView = mkView env translations locationFileSection content insertSection <$> bModel

    -- can kun være fordi vi forker og opdatere samme ting
    view <- UI.div # set children [locationFileSection, content, insertSection]

--------------------------------------------------------------------------------

    let eModel = rumors $ tModel
    _ <- onEvent eModel $ \m -> do
        void $ liftIO $ do
            case toJust (unModel m) of
                Nothing -> return ()
                Just i  -> do
                    --Location.writeLocationFile mLocationConfigFile (location i)
                    Grade.writeGrades mGradesFile (grades i)
                    return ()

    tabs'      <- mkElement "nav" #. "section" #+ [mkTabs env tabs]
    navigation <-
        mkElement "footer" #. "section" #+ [mkNavigation env translations tabs]

    void $ UI.getBody win # set children [tabs', view, navigation]

    UI.setFocus (getElement input) -- Can only do this if element exists and should not do this if not focus


---udskift med monoid instance..
mkView env translations x y z model =
        case unModel model of
            NotAsked -> do
                Lens.views starting string translations
            Loading -> do
                Lens.views loading string translations
            Failure e -> do
                Lens.views locationPageError string translations
            Data data' -> do
                UI.div # set children [x,y,z]


locationFileView :: Env -> Translation -> Location.LocationFile -> [UI Element]
locationFileView Env {..} translations locationFile = do
    let title_  = UI.div #+ [Lens.views locationTitle string translations]
    let content = UI.div #+ [UI.string (Location.unLocationFile locationFile)]


    let make = mkFileMaker "locationsPicker" (Lens.view newLocation translations)
            $ \file -> when (file /= "") $ void $ Location.writeLocationFile mLocationConfigFile (Location.LocationFile file)

    let pick = mkFilePicker "locationFilePicker" (Lens.view pickLocation translations)
                    $ \file -> when (file /= "") $ void $ Location.writeLocationFile  mLocationConfigFile (Location.LocationFile file)

    let pickers = UI.div #. "buttons has-addons" #+ [pick, make]

    let open = mkOpenFile "open"
                          (Lens.view openLocation translations)
                          (Location.unLocationFile locationFile)

    [title_, content, pickers, open]



inputGrade :: String -> Grade.Grades -> Grade.Grades
inputGrade name grades = Grade.Grades
    (ListZipper.mapFocus (const (Grade.Grade name)) (Grade.unGrades grades))


mkNewGrade :: Grade.Grades -> Grade.Grades
mkNewGrade grades =
    Grade.Grades $ ListZipper.insert (Grade.unGrades grades) (Grade.Grade "")


selectGrade :: Int -> Grade.Grades -> Grade.Grades
selectGrade selected grades =
        -- TODO this just wierd
    fromMaybe grades $ asum $ ListZipper.toNonEmpty $ ListZipper.iextend
        (\thisIndex grades' -> if selected == thisIndex
            then Just (Grade.Grades grades')
            else Nothing
        )
        (Grade.unGrades grades)



mkGrades :: Env -> Grade.Grades -> [UI Element]
mkGrades env (Grade.Grades grades') = do
    let elems = ListZipper.iextend
            (\index grades'' -> (index, grades' == grades'', extract grades''))
            grades'

    map (mkGrade env) (ListZipper.toList elems)



mkGrade :: Env -> (Int, Bool, Grade.Grade) -> UI Element
mkGrade Env {..} (thisIndex, isCenter, grade) = do
    let name   = Grade.unGrade grade
    let option = UI.option # set value (show thisIndex) # set text name
    if isCenter then option # set UI.selected True else option
