module Lib.Server.Server
    ( run
    ) where

import Graphics.UI.Threepenny.Core 
import qualified Graphics.UI.Threepenny as UI

import Control.Concurrent.MVar (withMVar)
import Lib.App (Env(..), Files(..))

import Lib.Tab
import Lib.Doneshooting
import Lib.Client.Dump
import Lib.Client.Doneshooting


import Utils.Comonad
import Utils.ListZipper (focus)

items :: WriteAttr Element (UI Element)
items = mkWriteAttr $ \item container -> void $
    element container # set children [] #+ [item]


tabsView :: Env -> Doneshooting -> Tabs -> UI Element
tabsView env doneshooting tabs =
    focus $ unTabs tabs =>> \tabs' ->
        let
            currentTab = focus tabs'
        in
            case currentTab of
                DumpTab -> dumpSection env tabs
                DoneshootingTab -> doneshootingSection env doneshooting tabs
                _ -> UI.div #+ [dumpSection env tabs] -- menus currentTab tabs



run :: Env -> UI.Event Doneshooting -> UI.Event Tabs -> IO ()
run env@Env{..} eDoneshooting eTabs = do
    tabs <- withMVar files $ \ Files{..} -> getTabs tabsFile
    doneshooting <- withMVar files $ \ Files{..} -> getDoneshooting doneshootingFile

    startGUI defaultConfig
        { jsWindowReloadOnDisconnect = False
        , jsStatic = Just "static"
        , jsCustomHTML = Just "index.html"
        } $ \win -> do
        -- behaviors
        bTabs <- stepper tabs eTabs
        bDoneshooting <- stepper doneshooting eDoneshooting

        list <- UI.div # sink items (liftA2 (tabsView env) bDoneshooting bTabs)

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

