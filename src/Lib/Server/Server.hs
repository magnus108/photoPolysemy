module Lib.Server.Server
    ( run
    ) where

import Graphics.UI.Threepenny.Core 
import qualified Graphics.UI.Threepenny as UI

import Control.Concurrent.MVar (withMVar)
import Lib.App (Env(..), Files(..))

import Lib.Tab
import Lib.Photographer
import Lib.Doneshooting
import Lib.Client.Dump
import Lib.Client.Doneshooting
import Lib.Client.Photographer


import Utils.ListZipper (focus)


items :: WriteAttr Element (UI Element)
items = mkWriteAttr $ \item container -> void $
    element container # set children [] #+ [item]


tabsView :: Env -> Doneshooting -> Photographers -> Tabs -> UI Element
tabsView env doneshooting photographers tabs =
    let
        --TODO this is silly
        currentTab = focus (unTabs tabs)
    in
        case currentTab of
            DumpTab -> dumpSection env tabs
            DoneshootingTab -> doneshootingSection env doneshooting tabs
            PhotographersTab -> photographersSection env photographers tabs
            _ -> UI.div #+ [dumpSection env tabs] -- menus currentTab tabs



run :: Int -> Env -> UI.Event Doneshooting -> UI.Event Tabs -> UI.Event Photographers -> IO ()
run port env@Env{..} eDoneshooting eTabs ePhotographers = do
    tabs <- withMVar files $ \ Files{..} -> getTabs tabsFile
    photographers <- withMVar files $ \ Files{..} -> getPhotographers photographersFile
    doneshooting <- withMVar files $ \ Files{..} -> getDoneshooting doneshootingFile

    startGUI defaultConfig
        { jsWindowReloadOnDisconnect = False
        , jsStatic = Just "static"
        , jsCustomHTML = Just "index.html"
        , jsPort = Just port
        } $ \win -> do
        -- behaviors
        bTabs <- stepper tabs eTabs
        bPhotographers <- stepper photographers ePhotographers
        bDoneshooting <- stepper doneshooting eDoneshooting

        list <- UI.div # sink items (liftA3 (tabsView env) bDoneshooting bPhotographers bTabs)

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

