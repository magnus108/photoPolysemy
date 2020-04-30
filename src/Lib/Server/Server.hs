module Lib.Server.Server
    ( run
    ) where

import Graphics.UI.Threepenny.Core 
import qualified Graphics.UI.Threepenny as UI

import Control.Concurrent.MVar (withMVar)
import Lib.App (Env(..), Files(..))

import Lib.Data
import Lib.Tab
import Lib.Photographer
import qualified Lib.Photographer as Photographer
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
import Lib.Client.Main


import Utils.ListZipper (focus)


view :: Env -> Window -> Event (Data String Grades) -> Event (Either String LocationFile) -> Event (Either String Sessions) -> Event (Either String Shootings) -> Event (Either String Cameras) -> Event (Either String Dump) -> Event (Data String DumpDir) -> Event (Either String Doneshooting) -> Event (Either String Dagsdato) -> Event (Either String DagsdatoBackup) -> UI.Behavior Photographer.Model -> Tabs -> UI ()
view env win eGrades eLocationConfigFile eSessions eShootings eCameras eDump eDumpDir eDoneshooting eDagsdato eDagsdatoBackup bPhotographers tabs = do
    let currentTab = focus (unTabs tabs)
    case currentTab of
        DumpTab -> dumpSection env win tabs eDump
        DoneshootingTab -> doneshootingSection env win eDoneshooting tabs
        PhotographersTab -> photographersSection env win tabs bPhotographers
        ShootingsTab -> shootingsSection env win eShootings tabs
        SessionsTab -> sessionsSection env win eSessions tabs
        CamerasTab -> camerasSection env win eCameras tabs
        DagsdatoTab -> dagsdatoSection env win eDagsdato tabs
        DagsdatoBackupTab -> dagsdatoBackupSection env win eDagsdatoBackup tabs
        LocationTab -> locationSection env win eLocationConfigFile eGrades tabs
        MainTab -> mainSection env win tabs eGrades eDump eDumpDir
        _ -> mainSection env win tabs eGrades eDump eDumpDir



run :: Int -> Env -> UI.Event (Data String Grades) ->  UI.Event (Either String LocationFile) -> UI.Event (Either String Sessions) -> UI.Event (Either String Shootings) -> UI.Event (Either String Cameras) -> UI.Event (Either String Dump) -> UI.Event (Data String DumpDir) -> UI.Event (Either String Doneshooting) -> UI.Event (Either String Dagsdato) -> UI.Event (Either String DagsdatoBackup) -> UI.Event Tabs -> UI.Behavior (Photographer.Model) -> IO ()
run port env@Env{..} eGrades eLocationConfigFile eSessions eShootings eCameras eDump eDumpDir eDoneshooting eDagsdato eDagsdatoBackup eTabs bPhotographers = do
    tabs <- withMVar files $ \ Files{..} -> getTabs tabsFile

    startGUI defaultConfig
        { jsWindowReloadOnDisconnect = False
        , jsStatic = Just "static"
        , jsCustomHTML = Just "index.html"
        , jsPort = Just port
        } $ \win -> do

        -- behaviors
        bTabs <- stepper tabs eTabs

        view env win eGrades eLocationConfigFile eSessions
                                            eShootings
                                            eCameras
                                            eDump
                                            eDumpDir
                                            eDoneshooting
                                            eDagsdato
                                            eDagsdatoBackup
                                            bPhotographers
                                            tabs

        UI.onChanges bTabs (view env win eGrades eLocationConfigFile eSessions
                                            eShootings
                                            eCameras
                                            eDump
                                            eDumpDir
                                            eDoneshooting
                                            eDagsdato
                                            eDagsdatoBackup
                                            bPhotographers)

