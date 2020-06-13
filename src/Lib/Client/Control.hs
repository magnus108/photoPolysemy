module Lib.Client.Control
    ( controlSection
    ) where


import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI


import qualified Lib.Client.Location as CLocation
import qualified Control.Lens as Lens

import           Lib.Client.Tab
import           Lib.Translation
import           Lib.Data
import           Lib.Tab
import qualified Lib.ControlModel as Model
import qualified Lib.Grade as Grade
import qualified Lib.Doneshooting as Doneshooting
import           Lib.App                        ( Env(..))
import Lib.Client.Utils
import Lib.Client.Element


mkControl :: Env -> Translation -> Element -> Model.Model -> UI Element
mkControl env@Env{..} translations select model =
    case Lens.view Model.unModel model of
        NotAsked -> UI.p #+ [Lens.views starting string translations]
        Loading -> UI.p #+ [Lens.views loading string translations]
        Failure _ -> do
            err <- UI.p #+ [Lens.views controlError string translations]
            UI.div #. "section" # set children [err]
        Data item' -> do
            counter <- UI.div #. "section" #+ [ mkLabel (Lens.view doneshootingDirCounter translations)
                                , UI.string (show $ Doneshooting.count (Lens.view Model.doneshootingDir item'))
                                ]
            
            let options = CLocation.mkGrades env (Lens.view Model.grades item')
            _ <- element select # set children [] #+ options
            selectGradeSection <- UI.div #. "section" #+ [UI.div #. "select" # set children [select]]
            UI.div # set children [counter, selectGradeSection]


controlSection :: Env -> Window -> Translation -> Tabs -> Behavior Model.Model -> UI ()
controlSection env@Env{..} win translation tabs bModel = do
    selectGrade <- UI.select
    let eSelect = CLocation.selectGrade <$> filterJust (selectionChange' selectGrade)
    let gradeEvent = concatenate' <$> unions' (eSelect :| [])
    let ee  = filterJust
                $   fmap
                        (\m f -> case toJust (Lens.view Model.unModel m) of
                            Nothing -> Nothing
                            Just x -> Just $ Model.Model $ Data $ Lens.over Model.grades f x
                        )
                        bModel
                <@> gradeEvent

    _ <- onEvent ee $ \model -> do
        void $ liftIO $ do
            case toJust (Lens.view Model.unModel model) of
                Nothing -> return ()
                Just item'  -> do
                    _ <- Grade.writeGrades mGradesFile (Lens.view Model.grades item')
                    return ()


    let bView = mkControl env translation selectGrade <$> bModel

    content <- UI.div # sink item bView


    tabs' <- mkElement "nav" #. "section" #+ [mkTabs env translation tabs]
    navigation <- mkElement "footer" #. "section" #+ [mkNavigation env translation tabs]

    view <- UI.div #+ fmap element [ content ]

    void $ UI.getBody win # set children [tabs', view, navigation]
