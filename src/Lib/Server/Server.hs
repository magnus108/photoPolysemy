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

import Lib.Client.Session
import Lib.Client.Shooting
import Lib.Client.Camera
import Lib.Client.Dump
import Lib.Client.Dagsdato
import Lib.Client.DagsdatoBackup
import Lib.Client.Doneshooting
import Lib.Client.Photographer


import Utils.ListZipper (focus)


items :: WriteAttr Element (UI Element)
items = mkWriteAttr $ \item container -> void $
    element container # set children [] #+ [item]


tabsView :: Env -> Sessions -> Shootings -> Cameras -> Dump -> Doneshooting -> Dagsdato -> DagsdatoBackup -> Photographers -> Tabs -> UI Element
tabsView env sessions shootings cameras dump doneshooting dagsdato dagsdatoBackup photographers tabs =
    let
        --TODO this is silly
        currentTab = focus (unTabs tabs)
    in
        case currentTab of
            DumpTab -> dumpSection env dump tabs
            DoneshootingTab -> doneshootingSection env doneshooting tabs
            PhotographersTab -> photographersSection env photographers tabs
            ShootingsTab -> shootingsSection env shootings tabs
            SessionsTab -> sessionsSection env sessions tabs
            CamerasTab -> camerasSection env cameras tabs
            DagsdatoTab -> dagsdatoSection env dagsdato tabs
            DagsdatoBackupTab -> dagsdatoBackupSection env dagsdatoBackup tabs
            _ -> UI.div #+ [dagsdatoSection env dagsdato tabs] -- menus currentTab tabs



run :: Int -> Env -> UI.Event Sessions -> UI.Event Shootings -> UI.Event Cameras -> UI.Event Dump -> UI.Event Doneshooting -> UI.Event Dagsdato -> UI.Event DagsdatoBackup -> UI.Event Tabs -> UI.Event Photographers -> IO ()
run port env@Env{..} eSessions eShootings eCameras eDump eDoneshooting eDagsdato eDagsdatoBackup eTabs ePhotographers = do
    tabs <- withMVar files $ \ Files{..} -> getTabs tabsFile
    photographers <- withMVar files $ \ Files{..} -> getPhotographers photographersFile
    cameras <- withMVar files $ \ Files{..} -> getCameras camerasFile
    sessions <- withMVar files $ \ Files{..} -> getSessions sessionsFile
    shootings <- withMVar files $ \ Files{..} -> getShootings shootingsFile
    doneshooting <- withMVar files $ \ Files{..} -> getDoneshooting doneshootingFile
    dagsdato <- withMVar files $ \ Files{..} -> getDagsdato dagsdatoFile
    dagsdatoBackup <- withMVar files $ \ Files{..} -> getDagsdatoBackup dagsdatoBackupFile
    dump <- withMVar files $ \ Files{..} -> getDump dumpFile

    startGUI defaultConfig
        { jsWindowReloadOnDisconnect = False
        , jsStatic = Just "static"
        , jsCustomHTML = Just "index.html"
        , jsPort = Just port
        } $ \win -> do

        -- behaviors
        bTabs <- stepper tabs eTabs
        bPhotographers <- stepper photographers ePhotographers
        bCameras <- stepper cameras eCameras
        bShootings <- stepper shootings eShootings
        bDoneshooting <- stepper doneshooting eDoneshooting
        bSessions <- stepper sessions eSessions
        bDagsdato <- stepper dagsdato eDagsdato
        bDagsdatoBackup <- stepper dagsdatoBackup eDagsdatoBackup
        bDump <- stepper dump eDump

        list <- UI.div # sink items (fmap (tabsView env)
                                            bSessions
                                            <*> bShootings
                                            <*> bCameras
                                            <*> bDump
                                            <*> bDoneshooting
                                            <*> bDagsdato
                                            <*> bDagsdatoBackup
                                            <*> bPhotographers
                                            <*> bTabs)

        void $ UI.getBody win #+
            fmap element [list]

        --bAccept <- stepper "" eAccept
        --entree <- UI.entry $ (\x -> x) <$> bAccept
        --_ <- element entree # set (attr "size") "10" # set style [("width","200px")]

        --myButton <- UI.button # set text "Click me!"

        --kig crud cuz this wont work
        --val <- liftIO $ withMVar files $ \ Files{..} -> do
        --   (Doneshooting path) <- getDoneshooting doneshooting
        ---newIORef path

        {--
        nick <- liftIO $ readIORef val
        names <- string nick

        UI.on UI.click myButton $ \_ -> liftIO $
            withMVar files $ \ Files{..} -> do
                (Doneshooting path) <- getDoneshooting doneshooting
                writeFile (path </> "foo.txt") "gg"

        --}

