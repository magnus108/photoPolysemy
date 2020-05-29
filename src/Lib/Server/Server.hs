module Lib.Server.Server
    ( run
    ) where

import Graphics.UI.Threepenny.Core 
import qualified Graphics.UI.Threepenny as UI

import Control.Concurrent.MVar (withMVar)
import Lib.App (Env(..), Files(..))

import Lib.Translation
import Lib.Tab
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

import qualified Lib.Client.Location as CLocation
import qualified Lib.Client.Main as CMain
import Lib.Client.Session
import Lib.Client.Shooting
import Lib.Client.Camera
import Lib.Client.Dump
import Lib.Client.Dagsdato
import Lib.Client.DagsdatoBackup
import Lib.Client.Doneshooting
import Lib.Client.Photographer


import Utils.ListZipper (focus)


view :: Env -> Window -> Translation -> Behavior Grade.Model -> Behavior Location.Model -> UI.Behavior Session.Model -> UI.Behavior Shooting.Model -> UI.Behavior Camera.Model -> UI.Behavior Dump.DumpModel -> UI.Behavior Dump.DumpDirModel -> UI.Behavior Doneshooting.Model -> UI.Behavior Dagsdato.Model -> UI.Behavior DagsdatoBackup.Model -> UI.Behavior Photographer.Model -> UI.Behavior Photographee.Model -> Handler (Grade.Model) -> Handler (Location.Model) -> Handler Dump.DumpModel -> Handler Dump.DumpDirModel -> Handler Photographee.Model -> Tabs -> UI ()
view env@Env{..} win translation bGrades bLocationConfigFile bSessions bShootings bCameras bDump bDumpDir bDoneshooting bDagsdato bDagsdatoBackup bPhotographers bPhotographees _ _ _ _ _ tabs = do
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
            let bModel = CMain.mkModel <$> bLocationConfigFile <*> bGrades <*> bDump <*> bDumpDir <*> bPhotographees
            CMain.mainSection env win translation tabs bModel
            -- QUICK BADNESS
            return ()

        _ -> dagsdatoBackupSection env win translation tabs bDagsdatoBackup



run :: Int -> Env -> Translation -> UI.Behavior Grade.Model ->  UI.Behavior Location.Model -> UI.Behavior Session.Model -> UI.Behavior Shooting.Model -> UI.Behavior Camera.Model -> UI.Behavior Dump.DumpModel -> UI.Behavior Dump.DumpDirModel -> UI.Behavior Doneshooting.Model -> UI.Behavior Dagsdato.Model -> UI.Behavior DagsdatoBackup.Model -> UI.Event Tabs -> UI.Behavior (Photographer.Model) -> UI.Behavior (Photographee.Model) -> Handler (Grade.Model) -> Handler (Location.Model) -> Handler Dump.DumpModel -> Handler Dump.DumpDirModel -> Handler Photographee.Model -> IO ()
run port env@Env{..} translations eGrades bLocationConfigFile eSessions eShootings eCameras eDump bDumpDir eDoneshooting eDagsdato eDagsdatoBackup eTabs bPhotographers bPhotographees hGrades hLocationConfigFile hDump hDumpDir hPhotographees = do
    tabs <- withMVar files $ \ Files{..} -> getTabs tabsFile

    startGUI defaultConfig
        { jsWindowReloadOnDisconnect = False
        , jsStatic = Just "static"
        , jsCustomHTML = Just "index.html"
        , jsPort = Just port
        } $ \win -> do

        -- behaviors
        bTabs <- stepper tabs eTabs

        view env win translations eGrades bLocationConfigFile eSessions
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

        UI.onChanges bTabs (view env win translations eGrades bLocationConfigFile eSessions
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

