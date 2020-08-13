module Lib.Client.Main
    ( mainSection
    , mkModel
    ) where
import Control.DeepSeq

import Lib.App (Action(..))
import qualified Control.Concurrent.Chan.Strict as Chan

import Lib.Data
import qualified Lib.Main as Main
import qualified Lib.Server.Build as SBuild
import qualified Lib.Build as Build

import Utils.Comonad
import qualified Utils.ListZipper as ListZipper

import           Data.Char
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Translation
import Lib.Tab
import qualified Lib.Photographer as Photographer
import qualified Lib.DagsdatoBackup as DagsdatoBackup
import qualified Lib.Doneshooting as Doneshooting
import qualified Lib.Shooting as Shooting
import qualified Lib.Photographee as Photographee
import qualified Lib.Grade as Grade
import qualified Lib.Dump as Dump
import qualified Lib.Session as Session
import qualified Lib.Location as Location
import qualified Lib.Camera as Camera
import qualified Lib.Dagsdato as Dagsdato

import Lib.Client.Tab
import qualified Lib.Client.Location as CLocation
import Lib.Client.Utils

import Lib.App (Env(..))

import qualified Control.Lens as Lens

import           Reactive.Threepenny
import Lib.Client.Element


mkModel :: Location.Model -> Grade.Model -> Dump.DumpModel -> Dump.DumpDirModel -> Photographee.Model -> Data String Session.Session -> Data String Camera.Camera -> Data String Dagsdato.Dagsdato -> Data String Shooting.Shooting -> Data String Doneshooting.Doneshooting -> Data String Photographer.Photographer -> Data String DagsdatoBackup.DagsdatoBackup -> Data String Build.Build -> Main.Model
mkModel location grades dump dumpDir photograhees session camera dagsdato shooting doneshooting photographer dagsdatoBackup build' =
    Main.Model $ Main.Item <$>
        Location.unModel location <*> Grade._grades grades <*>
            Dump.unModel dump <*>
                Dump.unDumpDirModel dumpDir <*>
                    (Lens.view Photographee.unModel photograhees)
                    <*> session
                    <*> camera
                    <*> dagsdato
                    <*> shooting
                    <*> doneshooting
                    <*> photographer
                    <*> dagsdatoBackup
                    <*> build'


photographeesList :: Env -> Window -> Dump.DumpDir -> Photographee.Photographees -> UI [Element]
photographeesList env _ dumpDir photographees' = do
        let currentPhotographee = extract (Photographee.toZip photographees')
        let elems = (Photographee.toZip photographees') =>> \photographees''-> let
                        thisPhotographee = extract photographees''
                    in
                        ( thisPhotographee
                        , thisPhotographee == currentPhotographee
                        , case photographees' of
                            (Photographee.CorrectPhotographees _) ->
                                Photographee.CorrectPhotographees photographees''
                            (Photographee.ChangedPhotographees _) ->
                                Photographee.ChangedPhotographees photographees''
                        )

        let elems' = sortOn (\(a,_,_) -> fmap toLower (Photographee.toName' a)) $ toList elems
        mapM (mkPhotographee env dumpDir) elems'


mkPhotographee :: Env -> Dump.DumpDir -> (Photographee.Photographee, Bool, Photographee.Photographees) -> UI Element
mkPhotographee Env{..} dumpDir (photographee, isCenter, photographees)
    | isCenter = do
        let name = Photographee.toName' photographee
        UI.div #. "section" #+ [mkButton name name #. "button is-success is-selected" # set (attr "disabled") "true"]
    | otherwise = do
        let name = Photographee.toName' photographee
        button <- mkButton name name
        UI.on UI.click button $ \_ ->
            liftIO $ Chan.writeChan chan $ ( WritePhotographees photographees dumpDir)
        UI.div #. "section" #+ [element button]



dumpFilesCounter :: Env -> Window -> Translation -> Dump.DumpDir -> UI Element
dumpFilesCounter _ _ translations dumpDir =
    UI.div #. "section" #+
        [ mkLabel (Lens.view dumpDirCounter translations)
        , UI.string (show $ Dump.count dumpDir)
                #. "is-size-1 has-text-danger has-text-weight-bold" # set (attr "id") "count"]



---------------------------------------------------------------------------------
mainSection :: Env -> Window -> Translation -> Tabs -> Behavior Main.Model -> UI ()
mainSection env@Env{..} win translations tabs bModel = do
    (input, view) <- sinkModel env win translations bModel

    tabs' <- mkElement "nav" #. "section" #+ [mkTabs env translations tabs]
    navigation <- mkElement "footer" #. "section" #+ [mkNavigation env translations tabs]

    void $ UI.getBody win # set children [tabs', view, navigation]
    liftIOLater $ do
        runUI win $ void $ do
            UI.setFocus input


setBuild :: Env -> Translation -> Element -> Session.Session -> UI ()
setBuild _ translations button session = do
    let name = Session.translationSessionButton session translations
    void $ element button # set text name

setChanged :: Env -> Translation -> Element -> Element -> Element -> Photographee.Photographees -> UI (Maybe Element)
setChanged _ translations content parent button photographees = do
    case photographees of
        Photographee.CorrectPhotographees ys  ->
            return Nothing
        Photographee.ChangedPhotographees _ -> do
            let msg = Lens.view changedPhotographeesError translations
            item <- element content #. "has-text-danger is-size-3" # set text msg
            val <- element parent # set children [item, button]
            return $ Just val


mkPhotographees :: Env -> Photographee.Photographees -> [UI Element]
mkPhotographees env photographees' = do
    let elems = ListZipper.iextend (\i photographees'' -> (i, (Photographee.toZip photographees') == photographees'', extract photographees'')) (Photographee.toZip photographees')
    map (mkPhotographeeListItem env) (ListZipper.toList elems)

mkPhotographeeListItem :: Env -> (Int, Bool, Photographee.Photographee) -> UI Element
mkPhotographeeListItem Env {..} (thisIndex, isCenter, photographee) = do
    let name   = Photographee.toName' photographee
    let option = UI.option # set value (show thisIndex) # set text name
    if isCenter then option # set UI.selected True else option

selectPhotographeeF :: Int -> Photographee.Photographees -> Photographee.Photographees
selectPhotographeeF selected photographees' =
        -- TODO this just wierd
    fromMaybe photographees' $ asum $ ListZipper.toNonEmpty $ ListZipper.iextend
        (\thisIndex photographees'' -> if selected == thisIndex
            then Just (Photographee.CorrectPhotographees photographees'')
            else Nothing
        ) (Photographee.toZip photographees')


sinkModel :: Env -> Window -> Translation -> Behavior Main.Model -> UI (Element, Element)
sinkModel env@Env{..} win translations bModel = do

    let isOk = Lens.view isChanged translations
    changedButton <- mkButton "mkChange" isOk
    changed' <- UI.p
    changed <- UI.div #. "section"


    mkBuild' <- mkButton "mkBuild" ""
    mkBuild <- UI.div #. "section" # set children [mkBuild']

    build' <- UI.div #. "section"

    photographerName <- UI.p #. "section has-text-info"

    content <- UI.div
    select <- UI.select
    input <- UI.input #. "input"
    currentPhotographee <- UI.h1 #. "is-size-4"

    help <- string "Valgt: "
    findHelp <- string "Søg: "
    inputSection <- UI.div #. "section" # set children [help, currentPhotographee, findHelp, input]

    selectSection <-
        UI.div
        #. "section"
        #+ [ UI.div
                #. "field is-horizontal"
                #+ [ UI.div
                    #. "field-body"
                    #+ [ UI.div
                        #. "field"
                        #+ [UI.p #. "control" #+ [UI.div #. "select" #+ [element select]]]
                        ]
                ]
            ]

    count <- UI.div

    bEditingInput <- bEditing input
    bEditingSelect  <- bEditing select

    selectPhotographee <- UI.select
    bEditingSelectPhotographee <- bEditing selectPhotographee

    ok <- UI.div

    liftIOLater $ do
        model <- currentValue bModel
        runUI win $ void $ do
            case Main._unModel model of
                NotAsked -> do
                    msg <- Lens.views starting string translations
                    _ <- element content # set children [msg]
                    return ()
                Loading -> do
                    msg <- Lens.views loading string translations
                    _ <- element content # set children [msg]
                    return ()
                Failure e -> do
                    msg <- Lens.views mainPageError string translations
                    err <- UI.div #+ [string e]
                    section <- UI.div #. "section" # set children [msg, err]
                    _ <- element content # set children [section]
                    return ()

                Data item' -> do
                    buildStatus <- UI.string $ Build.toString (Main._build item') translations
                    _ <- element photographerName # set text (Lens.view Photographer.name (Main._photographer item'))
                    _ <- element build' # set children [buildStatus]
                    _ <- case (Main._build item') of
                            (Build.DoneBuild _ _) -> do
                                runFunction  $ ffi "$(%1).removeAttr('disabled')" (mkBuild')
                            (Build.Building _ _ ) ->void$ element mkBuild' # set (attr "disabled") "true"
                            (Build.NoBuild) -> 
                                runFunction  $ ffi "$(%1).removeAttr('disabled')" (mkBuild')

                    _ <- case (Main._photographees item') of
                       (Photographee.ChangedPhotographees _) ->
                           void$ element mkBuild' # set (attr "disabled") "true"
                       (Photographee.CorrectPhotographees _) ->
                            runFunction  $ ffi "$(%1).removeAttr('disabled')" (mkBuild')

                    _ <- setBuild env translations mkBuild' (Main._session item')

                    dumpFilesCounter' <- dumpFilesCounter env win translations (Main._dumpDir item')
                    _ <- element count # set children [dumpFilesCounter']
                    let options = CLocation.mkGrades env (Main._grades item')
                    _ <- element select # set children [] #+ options

                    photographeesList' <- photographeesList env win (Main._dumpDir item') (Main._photographees item')

                    let _ = Photographee.toIdent (Main._photographees item')
                    let name = Photographee.toName (Main._photographees item')
                    _ <- element currentPhotographee # set text name

                    isChanged <- setChanged env translations changed' changed changedButton (Main._photographees item')
                    element ok # set children (maybeToList isChanged)


                    _ <- element selectPhotographee # set children [] #+ (mkPhotographees env (Main._photographees item'))
                    photographees' <- UI.div #. "section"
                                    #+ [ UI.div
                                        #. "field"
                                        #+ [ UI.p #. "control" #+ [UI.div #. "select" #+ [element selectPhotographee]]]
                                    ]

                    _ <- element content # set children ([photographerName, build', mkBuild, count, ok, inputSection, selectSection, photographees'])
                    return ()


    liftIOLater $ onChange bModel $ \model -> runUI win $ do
        case Main._unModel model of
            NotAsked -> do
                msg <- Lens.views starting string translations
                _ <- element content # set children [msg]
                return ()
            Loading -> do
                msg <- Lens.views loading string translations
                _ <- element content # set children [msg]
                return ()
            Failure e -> do
                msg <- Lens.views mainPageError string translations
                err <- UI.div #+ [string e]
                section <- UI.div #. "section" # set children [msg, err]
                _ <- element content # set children [section]
                return ()
            Data item' -> do
                dumpFilesCounter' <- dumpFilesCounter env win translations (Main._dumpDir item')
                _ <- element count # set children [dumpFilesCounter']

                _ <- case (Main._build item') of
                        (Build.DoneBuild _ _) ->
                            runFunction  $ ffi "$(%1).removeAttr('disabled')" (mkBuild')
                        (Build.Building _ _ ) ->void$ element mkBuild' # set (attr "disabled") "true"
                        (Build.NoBuild) -> 
                            runFunction  $ ffi "$(%1).removeAttr('disabled')" (mkBuild')

                _ <- case (Main._photographees item') of
                       (Photographee.ChangedPhotographees _) ->
                            void$ element mkBuild' # set (attr "disabled") "true"
                       (Photographee.CorrectPhotographees _) ->
                            runFunction  $ ffi "$(%1).removeAttr('disabled')" (mkBuild')

                _ <- element photographerName # set text (Lens.view Photographer.name (Main._photographer item'))
                editingInput <- liftIO $ currentValue bEditingInput
                editingSelect <- liftIO $ currentValue bEditingSelect

                buildStatus <- UI.string $ Build.toString (Main._build item') translations
                _ <- element build' # set children [buildStatus]

                photographeesList' <- photographeesList env win (Main._dumpDir item') (Main._photographees item')

                editingSelectPhotographee <- liftIO $ currentValue bEditingSelectPhotographee

                when (not editingSelectPhotographee) $ void $
                    element selectPhotographee # set children [] #+ (mkPhotographees env (Main._photographees item'))

                let _ = Photographee.toIdent (Main._photographees item')
                let name = Photographee.toName (Main._photographees item')
                _ <- element currentPhotographee # set text name

                _ <- setBuild env translations mkBuild' (Main._session item')
                isChanged <- setChanged env translations changed' changed changedButton (Main._photographees item')
                element ok # set children (maybeToList isChanged)


                when (not editingSelect) $ void $ do
                    let options = CLocation.mkGrades env (Main._grades item')
                    element select # set children [] #+ options

                when (not editingInput) $ void $
                    element input # set value ""

                when (not (editingInput || editingSelect )) $ void $ do
                    photographees' <- UI.div #. "section"
                        #+ ([ UI.div
                                    #+ [ UI.div
                                        #. "field"
                                        #+ [ UI.p #. "control" #+ [UI.div #. "select" #+ [element selectPhotographee]]]
                                    ]
                                    ])
                    _ <- element content # set children ([photographerName, build', mkBuild, count, ok, inputSection, selectSection, photographees'])
                    UI.setFocus input
                    return ()


    let eSelect   = selectPhotographeeF <$> filterJust (selectionChange' selectPhotographee)

    let eSelectGrade = CLocation.selectGrade <$> filterJust (selectionChange' select)
    let eFind = Photographee.tryFindById <$> UI.valueChange input

    let gradeEvent = concatenate' <$> unions' (eSelectGrade :| [])
    let findEvent = concatenate' <$> unions' (eFind :| [])
    let ee  = filterJust
                $   fmap
                        (\m f -> case toJust (Main._unModel m) of
                            Nothing -> Nothing
                            Just x -> Just $ Main.Model $ Data $ Lens.over Main.grades f x
                        )
                        bModel
                <@> gradeEvent


    let ee2 = filterJust
                $   fmap
                        (\m f -> case toJust (Main._unModel m) of
                            Nothing -> Nothing
                            Just x -> Just $ Main.Model $ Data $ Lens.over Main.photographees f x
                        )
                        bModel
                <@> findEvent

    let ee3 = filterJust
                $   fmap
                        (\m f -> case toJust (Main._unModel m) of
                            Nothing -> Nothing
                            Just x -> Just $ Main.Model $ Data $ Lens.over Main.photographees f x
                        )
                        bModel
                <@> eSelect



    _ <- onEvent ee3 $ \model -> do
        void $ liftIO $ do
            case toJust (Main._unModel model) of
                Nothing -> return ()
                Just item'  -> do
                    --Location.writeLocationFile mLocationConfigFile (location i)
                    _ <- Chan.writeChan chan (WritePhotographees (Main._photographees item') (Main._dumpDir item'))
                    return ()


    let enterKeydown = filterJust $ (\keycode -> if (keycode == 13) then Just () else Nothing) <$> (UI.keydown input)

    let buildClick = seq <$> UI.click mkBuild'

    let changeOk = UI.click changedButton

    let ee5 = filterJust
                $   fmap
                        (\m -> case toJust (Main._unModel m) of
                            Nothing -> Nothing
                            Just x -> Just (Main.Model (Data x))
                        )
                        bModel
                <@ changeOk

    _ <- onEvent ee5 $ \model -> do
        void $ liftIO $ do
            case toJust (Main._unModel model) of
                Nothing -> return ()
                Just item'  -> do
                    _ <- Chan.writeChan chan $ ( WritePhotographeesOK (Main._photographees item') )
                    return ()

    let buildEvent = concatenate' <$> unions' (buildClick :| [fmap const enterKeydown])

    let ee4 = filterJust
                $   fmap
                        (\m -> case toJust (Main._unModel m) of
                            Nothing -> Nothing
                            Just x -> Just (Main.Model (Data x))
                        )
                        bModel
                <@ buildEvent



    _ <- onEvent ee4 $ \model -> do
        UI.setFocus (help) -- hack
        _ <- element input # set value ""
        void $ liftIO $ do
            case toJust (Main._unModel model) of
                Nothing -> return ()
                Just item'  -> do
                    case (Main._photographees item') of
                        (Photographee.CorrectPhotographees ys) -> do
                            _ <- Chan.writeChan chan $ ( MFcker (item'))
                            return ()
                        (Photographee.ChangedPhotographees ys) -> do
                            return ()


    _ <- onEvent ee2 $ \model -> do
        void $ liftIO $ do
            case toJust (Main._unModel model) of
                Nothing -> return ()
                Just item'  -> do
                    --Location.writeLocationFile mLocationConfigFile (location i)
                    model2 <- currentValue bModel
                    case toJust (Main._unModel model2) of
                        Nothing -> do
                                _ <- Chan.writeChan chan $ ( WritePhotographees (Main._photographees item') (Main._dumpDir item'))
                                return $ ()
                        Just item'' -> do
                            if ((Main._photographees item') /= (Main._photographees item'')) then
                                void $ Chan.writeChan chan $ ( WritePhotographees (Main._photographees item') (Main._dumpDir item'))
                            else
                                return $ ()

    _ <- onEvent ee $ \model -> do
        void $ liftIO $ do
            case toJust (Main._unModel model) of
                Nothing -> return ()
                Just item'  -> do
                    --Location.writeLocationFile mLocationConfigFile (location i)
                    _ <- Chan.writeChan chan $ (WriteGrades ( Main._grades item'))
                    return ()

    return (input, content)
