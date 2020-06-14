module Lib.Server.Server
    ( run
    ) where

import System.FilePath
import Graphics.UI.Threepenny.Core 
import qualified Graphics.UI.Threepenny as UI

import Control.Concurrent.MVar (withMVar)
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


import qualified Utils.RoseTree as RT
import qualified Utils.TreeZipper as TZ

import Utils.ListZipper (focus)
import Utils.Comonad



view :: Env -> Window -> Translation -> Behavior Doneshooting.DoneshootingDirModel -> Behavior Build.Model -> Behavior Grade.Model -> Behavior Location.Model -> UI.Behavior Session.Model -> UI.Behavior Shooting.Model -> UI.Behavior Camera.Model -> UI.Behavior Dump.DumpModel -> UI.Behavior Dump.DumpDirModel -> UI.Behavior Doneshooting.Model -> UI.Behavior Dagsdato.Model -> UI.Behavior DagsdatoBackup.Model -> UI.Behavior Photographer.Model -> UI.Behavior Photographee.Model -> Handler (Grade.Model) -> Handler (Location.Model) -> Handler Dump.DumpModel -> Handler Dump.DumpDirModel -> Handler Photographee.Model -> Tabs -> UI ()
view env@Env{..} win translation bDoneshootingDir bBuild bGrades bLocationConfigFile bSessions bShootings bCameras bDump bDumpDir bDoneshooting bDagsdato bDagsdatoBackup bPhotographers bPhotographees _ _ _ _ _ tabs = do
    let currentTab = focus (unTabs tabs)
    case currentTab of
        DumpTab -> dumpSection env win translation tabs bDump
        DoneshootingTab -> doneshootingSection env win translation tabs bDoneshooting
        PhotographersTab -> photographersSection env win translation tabs bPhotographers
        ShootingsTab -> shootingsSection env win translation tabs bShootings
        SessionsTab -> sessionsSection env win translation tabs bSessions
        CamerasTab -> camerasSection env win translation tabs bCameras
        DagsdatoTab -> dagsdatoSection env win translation tabs bDagsdato
        DagsdatoBackupTab -> dagsdatoBackupSection env win translation tabs bDagsdatoBackup
        LocationTab -> do
            let bModel = liftA2 CLocation.mkModel bLocationConfigFile bGrades
            CLocation.locationSection env win translation tabs bModel
            -- QUICK BADNESS
            return ()

        MainTab -> do
            let bSession =
                    fmap (\x -> (\(Session.Sessions sessions) ->
                    case sessions of
                            (TZ.TreeZipper (RT.Leaf y) _) -> do
                                Data y
                            (TZ.TreeZipper (RT.Branch _ _) _) -> do
                                Failure "Session"
                                ) =<< (Session.unModel x) ) bSessions


            let bCamera = (\camerasData -> fmap (\(Camera.Cameras x) -> extract x) (Camera.unModel camerasData)) <$> bCameras
            let bDagsdato' = Dagsdato.unModel <$> bDagsdato
            let bShooting = fmap (\(Shooting.Shootings x) -> extract x) <$> Shooting.unModel <$> bShootings
            let bPhotographer = fmap (\(Photographer.Photographers x) -> extract x) <$> Photographer.unModel <$> bPhotographers
            let bDoneshooting' = Doneshooting.unModel <$> bDoneshooting
            let bDagsdatoBackup' = DagsdatoBackup.unModel <$> bDagsdatoBackup
            let bBuild' = Build.unModel <$> bBuild
            let bModel = CMain.mkModel <$> bLocationConfigFile <*> bGrades <*> bDump <*> bDumpDir <*> bPhotographees <*> bSession <*> bCamera <*> bDagsdato' <*> bShooting <*> bDoneshooting' <*> bPhotographer <*> bDagsdatoBackup' <*> bBuild'
            CMain.mainSection env win translation tabs bModel
            -- QUICK BADNESS
            return ()
        ControlTab -> do
            let bModel = ControlModel.mkModel <$> bGrades <*> bDoneshootingDir
            controlSection env win translation tabs bModel
            return ()

        InsertPhotographeeTab -> do
            let bModel = InsertPhotographee.mkModel <$> bLocationConfigFile <*> bGrades <*> bPhotographees
            InsertPhotographee.insertPhotographeeSection env win translation tabs bModel
            -- QUICK BADNESS
            return ()
        _ -> 
            return ()





run :: Int -> Env -> Translation -> UI.Behavior Doneshooting.DoneshootingDirModel -> UI.Behavior Build.Model -> UI.Behavior Grade.Model ->  UI.Behavior Location.Model -> UI.Behavior Session.Model -> UI.Behavior Shooting.Model -> UI.Behavior Camera.Model -> UI.Behavior Dump.DumpModel -> UI.Behavior Dump.DumpDirModel -> UI.Behavior Doneshooting.Model -> UI.Behavior Dagsdato.Model -> UI.Behavior DagsdatoBackup.Model -> UI.Event Tabs -> UI.Behavior (Photographer.Model) -> UI.Behavior (Photographee.Model) -> Handler (Grade.Model) -> Handler (Location.Model) -> Handler Dump.DumpModel -> Handler Dump.DumpDirModel -> Handler Photographee.Model -> IO ()
run port env@Env{..} translations bDoneshootingDir bBuild eGrades bLocationConfigFile eSessions eShootings eCameras eDump bDumpDir eDoneshooting eDagsdato eDagsdatoBackup eTabs bPhotographers bPhotographees hGrades hLocationConfigFile hDump hDumpDir hPhotographees = do
    tabs <- withMVar mTabsFile $ \tabsFile -> getTabs tabsFile

    startGUI defaultConfig
        { jsWindowReloadOnDisconnect = False
        , jsPort = Just port
        , jsStatic = Just (serverRoot </> "static")
        , jsCustomHTML = Just "index.html"
        } $ \win -> do

        -- behaviors
        bTabs <- stepper tabs eTabs

        view env win translations bDoneshootingDir bBuild eGrades bLocationConfigFile eSessions
                                            eShootings
                                            eCameras
                                            eDump
                                            bDumpDir
                                            eDoneshooting
                                            eDagsdato
                                            eDagsdatoBackup
                                            bPhotographers
                                            bPhotographees
                                            hGrades
                                            hLocationConfigFile
                                            hDump
                                            hDumpDir
                                            hPhotographees
                                            tabs

        UI.onChanges bTabs (view env win translations bDoneshootingDir bBuild eGrades bLocationConfigFile eSessions
                                            eShootings
                                            eCameras
                                            eDump
                                            bDumpDir
                                            eDoneshooting
                                            eDagsdato
                                            eDagsdatoBackup
                                            bPhotographers
                                            bPhotographees
                                            hGrades hLocationConfigFile
                                            hDump
                                            hDumpDir
                                            hPhotographees
                           )

