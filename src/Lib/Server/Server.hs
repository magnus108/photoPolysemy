module Lib.Server.Server
    ( run
    ) where

import Control.Concurrent (ThreadId, killThread)
import           Reactive.Threepenny

import Lib.Client.Tab
import System.FilePath
import Graphics.UI.Threepenny.Core 
import qualified Graphics.UI.Threepenny as UI

import Control.Concurrent.MVar.Strict (withMVar)
import Lib.App (Env(..))

import Lib.Translation
import Lib.Tab
import qualified Lib.Build as Build
import qualified Lib.Photographer as Photographer
import qualified Lib.Photographee as Photographee
import qualified Lib.Dump as Dump
import qualified Lib.Session as Session
import qualified Lib.Shooting as Shooting
import qualified Lib.Dagsdato as Dagsdato
import qualified Lib.DagsdatoBackup as DagsdatoBackup
import qualified Lib.Doneshooting as Doneshooting
import qualified Lib.Camera as Camera
import qualified Lib.Location as Location
import qualified Lib.Grade as Grade

import qualified Lib.ControlModel as ControlModel
import qualified Lib.Client.Location as CLocation
import qualified Lib.Client.InsertPhotographee as InsertPhotographee
import qualified Lib.Client.Main as CMain
import Lib.Client.Session
import Lib.Client.Shooting
import Lib.Client.Camera
import Lib.Client.Dump
import Lib.Client.Dagsdato
import Lib.Client.DagsdatoBackup
import Lib.Client.Doneshooting
import Lib.Client.Photographer
import Lib.Client.Control



import Utils.ListZipper (focus)
import Utils.Comonad



--view :: Env -> Window -> Translation -> Behavior Doneshooting.DoneshootingDirModel -> Behavior Build.Model -> Behavior Grade.Model -> Behavior Location.Model -> UI.Behavior Session.Model -> UI.Behavior Shooting.Model -> UI.Behavior Camera.Model -> UI.Behavior Dump.DumpModel -> UI.Behavior Dump.DumpDirModel -> UI.Behavior Doneshooting.Model -> UI.Behavior Dagsdato.Model -> UI.Behavior DagsdatoBackup.Model -> UI.Behavior Photographer.Model -> UI.Behavior Photographee.Model -> Handler (Grade.Model) -> Handler (Location.Model) -> Handler Dump.DumpModel -> Handler Dump.DumpDirModel -> Handler Photographee.Model -> Tabs -> UI ()
--view env@Env{..} win translation bDoneshootingDir bBuild bGrades bLocationConfigFile bSessions bShootings bCameras bDump bDumpDir bDoneshooting bDagsdato bDagsdatoBackup bPhotographers bPhotographees tabs  bModelLocation1 bModelInserter1 bModel1 bModel2= do





run :: Int -> Env -> Translation -> UI.Behavior Doneshooting.DoneshootingDirModel -> UI.Behavior Build.Model -> UI.Behavior Grade.Model ->  UI.Behavior Location.Model -> UI.Behavior Session.Model -> UI.Behavior Shooting.Model -> UI.Behavior Camera.Model -> UI.Behavior Dump.DumpModel -> UI.Behavior Dump.DumpDirModel -> UI.Behavior Doneshooting.Model -> UI.Behavior Dagsdato.Model -> UI.Behavior DagsdatoBackup.Model -> UI.Event Tabs -> UI.Behavior (Photographer.Model) -> UI.Behavior (Photographee.Model) -> ThreadId -> IO ()
run port env@Env{..} translations bDoneshootingDir bBuild eGrades bLocationConfigFile eSessions eShootings eCameras eDump bDumpDir eDoneshooting eDagsdato eDagsdatoBackup eTabs bPhotographers bPhotographees messageReceiver= do
    tabs <- withMVar mTabsFile $ \tabsFile -> getTabs tabsFile

    startGUI defaultConfig
        { jsWindowReloadOnDisconnect = False
        , jsPort = Just port
        , jsStatic = Just (serverRoot </> "static")
        , jsCustomHTML = Just "index.html"
        } $ \win -> do

        -- behaviors
        bTabs <- stepper tabs eTabs

        let bModelLocation1 = liftA2 CLocation.mkModel bLocationConfigFile eGrades
        let bModelInserter1 = InsertPhotographee.mkModel <$> bLocationConfigFile <*> eGrades <*> bPhotographees <*> bDumpDir

        let bSession1 =
                fmap (\x -> (\(Session.Sessions sessions) -> extract sessions
                            ) <$> (Session.unModel x) ) eSessions

        let bCamera1 = (\camerasData -> fmap (\(Camera.Cameras x) -> extract x) (Camera.unModel camerasData)) <$> eCameras
        let bDagsdato1' = Dagsdato.unModel <$> eDagsdato
        let bShooting1 = fmap (\(Shooting.Shootings x) -> extract x) <$> Shooting.unModel <$> eShootings
        let bPhotographer1= fmap (\(Photographer.Photographers x) -> extract x) <$> Photographer.unModel <$> bPhotographers
        let bDoneshooting1' = Doneshooting.unModel <$> eDoneshooting
        let bDagsdatoBackup1' = DagsdatoBackup.unModel <$> eDagsdatoBackup
        let bBuild1' = Build.unModel <$> bBuild
        let bModel1 = CMain.mkModel <$> bLocationConfigFile <*> eGrades <*> eDump <*> bDumpDir <*> bPhotographees <*> bSession1 <*> bCamera1 <*> bDagsdato1' <*> bShooting1 <*> bDoneshooting1' <*> bPhotographer1 <*> bDagsdatoBackup1' <*> bBuild1'


        let bModel2 = ControlModel.mkModel <$> eGrades <*> bDoneshootingDir <*> bPhotographees


        tabs' <- mkElement "nav" #. "section" #+ [mkTabs env translations tabs]
        navigation <- mkElement "footer" #. "section" #+ [mkNavigation env translations tabs]
        inputteren <- UI.input #. "input"


        contentInner <- UI.div
        mainSection' <- CMain.mainSection env win translations contentInner tabs' navigation bModel1 inputteren

        dumpSection' <- dumpSection env win translations tabs' navigation eDump
        doneshootingSection' <- doneshootingSection env win translations tabs' navigation eDoneshooting
        photographersSection' <- photographersSection env win translations tabs' navigation bPhotographers
        shootingsSection' <- shootingsSection env win translations tabs' navigation eShootings
        sessionsSection' <- sessionsSection env win translations tabs' navigation eSessions
        camerasSection' <- camerasSection env win translations tabs' navigation eCameras
        dagsdatoSection' <- dagsdatoSection env win translations tabs' navigation eDagsdato
        dagsdatoBackupSection' <- dagsdatoBackupSection env win translations tabs' navigation  eDagsdatoBackup
        locationSection' <- CLocation.locationSection env win translations tabs' navigation bModelLocation1
        controlSection' <- controlSection env win translations tabs' navigation bModel2
        insertPhotographeeSection' <- InsertPhotographee.insertPhotographeeSection env win translations tabs' navigation bModelInserter1

        content <- UI.div
        liftIOLater $ do
            model <- currentValue bTabs
            runUI win $ void $ do
                let currentTab = focus (unTabs model)
                let childe = case currentTab of
                        DumpTab -> dumpSection'
                        DoneshootingTab -> doneshootingSection'
                        PhotographersTab -> photographersSection'
                        ShootingsTab -> shootingsSection'
                        SessionsTab -> sessionsSection'
                        CamerasTab -> camerasSection'
                        DagsdatoTab -> dagsdatoSection'
                        DagsdatoBackupTab -> dagsdatoBackupSection'
                        LocationTab -> locationSection'
                        MainTab -> mainSection'
                        ControlTab -> controlSection'
                        InsertPhotographeeTab -> insertPhotographeeSection'

                tt <- mkTabs env translations (model)
                ttt <- mkNavigation env translations (model)
                element tabs' # set children [tt]
                element navigation # set children [ttt]

                void $ element content # set children [tabs', childe, navigation]

                case currentTab of
                    MainTab -> UI.setFocus (getElement inputteren)
                    _ -> return ()


        liftIOLater $ onChange bTabs $ \tabs'' -> runUI win $ do
            let currentTab = focus (unTabs tabs'')
            let childe = case currentTab of
                    DumpTab -> dumpSection'
                    DoneshootingTab -> doneshootingSection'
                    PhotographersTab -> photographersSection'
                    ShootingsTab -> shootingsSection'
                    SessionsTab -> sessionsSection'
                    CamerasTab -> camerasSection'
                    DagsdatoTab -> dagsdatoSection'
                    DagsdatoBackupTab -> dagsdatoBackupSection'
                    LocationTab -> locationSection'
                    MainTab -> mainSection'
                    ControlTab -> controlSection'
                    InsertPhotographeeTab -> insertPhotographeeSection'


            tt <- mkTabs env translations (tabs'')
            ttt <- mkNavigation env translations (tabs'')
            element tabs' # set children [tt]
            element navigation # set children [ttt]

            
            void $ element content # set children [tabs',childe, navigation]

            case currentTab of
                    MainTab -> UI.setFocus (getElement inputteren)
                    _ -> return ()

        void $ UI.getBody win # set children [content]

        UI.on UI.disconnect win $ const $ liftIO $ do
            killThread messageReceiver

