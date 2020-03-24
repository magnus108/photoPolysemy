module Lib.Server.Server
    ( run
    ) where

import Graphics.UI.Threepenny.Core 
import qualified Graphics.UI.Threepenny as UI

import Control.Concurrent.MVar (withMVar)
import Lib.App (Env(..), Files(..))

import Lib.Tab
import Lib.Photographer
import Lib.Camera
import Lib.Shooting
import Lib.Doneshooting
import Lib.Dump
import Lib.Dagsdato
import Lib.DagsdatoBackup
import Lib.Session
import Lib.Location
import Lib.Grade

import Lib.Client.Location
import Lib.Client.Session
import Lib.Client.Shooting
import Lib.Client.Camera
import Lib.Client.Dump
import Lib.Client.Dagsdato
import Lib.Client.DagsdatoBackup
import Lib.Client.Doneshooting
import Lib.Client.Photographer


import Utils.ListZipper (focus)


view :: Env -> Window -> Event Grades -> Behavior LocationFile -> Behavior Sessions -> Behavior Shootings -> Behavior Cameras -> Behavior Dump -> Behavior Doneshooting -> Behavior Dagsdato -> Behavior DagsdatoBackup -> Event (Either String Photographers) -> Tabs -> UI ()
view env win eGrades bLocationFile bSessions bShootings bCameras bDump bDoneshooting bDagsdato bDagsdatoBackup ePhotographers tabs = do
    let currentTab = focus (unTabs tabs)
    case currentTab of
        DumpTab -> dumpSection env win bDump tabs
        DoneshootingTab -> doneshootingSection env win bDoneshooting tabs
        PhotographersTab -> photographersSection env win ePhotographers tabs
        ShootingsTab -> shootingsSection env win bShootings tabs
        SessionsTab -> sessionsSection env win bSessions tabs
        CamerasTab -> camerasSection env win bCameras tabs
        DagsdatoTab -> dagsdatoSection env win bDagsdato tabs
        DagsdatoBackupTab -> dagsdatoBackupSection env win bDagsdatoBackup tabs
        LocationTab -> locationSection env win bLocationFile eGrades tabs
        _ -> return ()




run :: Int -> Env -> UI.Event Grades ->  UI.Event LocationFile -> UI.Event Sessions -> UI.Event Shootings -> UI.Event Cameras -> UI.Event Dump -> UI.Event Doneshooting -> UI.Event Dagsdato -> UI.Event DagsdatoBackup -> UI.Event Tabs -> UI.Event (Either String Photographers) -> IO ()
run port env@Env{..} eGrades eLocationConfigFile eSessions eShootings eCameras eDump eDoneshooting eDagsdato eDagsdatoBackup eTabs ePhotographers = do
    tabs <- withMVar files $ \ Files{..} -> getTabs tabsFile
    cameras <- withMVar files $ \ Files{..} -> getCameras camerasFile
    sessions <- withMVar files $ \ Files{..} -> getSessions sessionsFile
    shootings <- withMVar files $ \ Files{..} -> getShootings shootingsFile
    doneshooting <- withMVar files $ \ Files{..} -> getDoneshooting doneshootingFile
    dagsdato <- withMVar files $ \ Files{..} -> getDagsdato dagsdatoFile
    dagsdatoBackup <- withMVar files $ \ Files{..} -> getDagsdatoBackup dagsdatoBackupFile
    dump <- withMVar files $ \ Files{..} -> getDump dumpFile
    locationFile <- withMVar files $ \ Files{..} -> getLocationFile locationConfigFile

    startGUI defaultConfig
        { jsWindowReloadOnDisconnect = False
        , jsStatic = Just "static"
        , jsCustomHTML = Just "index.html"
        , jsPort = Just port
        } $ \win -> do

        -- behaviors
        bTabs <- stepper tabs eTabs
        bCameras <- stepper cameras eCameras
        bShootings <- stepper shootings eShootings
        bDoneshooting <- stepper doneshooting eDoneshooting
        bSessions <- stepper sessions eSessions
        bDagsdato <- stepper dagsdato eDagsdato
        bDagsdatoBackup <- stepper dagsdatoBackup eDagsdatoBackup
        bDump <- stepper dump eDump
        bLocationFile <- stepper locationFile eLocationConfigFile


 
        view env win eGrades bLocationFile bSessions
                                            bShootings
                                            bCameras
                                            bDump
                                            bDoneshooting
                                            bDagsdato
                                            bDagsdatoBackup
                                            ePhotographers
                                            tabs

        UI.onChanges bTabs (view env win eGrades bLocationFile bSessions
                                            bShootings
                                            bCameras
                                            bDump
                                            bDoneshooting
                                            bDagsdato
                                            bDagsdatoBackup
                                            ePhotographers)

