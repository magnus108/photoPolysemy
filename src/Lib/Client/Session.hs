module Lib.Client.Session
    ( sessionsSection
    ) where


import Data.Bitraversable

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Translation
import Lib.Tab
import Lib.Session
import Lib.Client.Tab

import Lib.Client.Utils
import Lib.App (Env(..),Files(..))
import Lib.Client.Element
import Control.Concurrent.MVar

import Utils.RoseTree as RT
import Utils.TreeZipper


sessionsSection :: Env -> Window -> Translation -> Event (Either String Sessions) -> Tabs -> UI ()
sessionsSection env@Env{..} win translation eSessions tabs = do
    sessions <- liftIO $ withMVar files $ \ Files{..} -> getSessions sessionsFile
    bSessions <- stepper sessions eSessions

    content <- UI.div # sink item (mkSessions env <$> bSessions)

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env translation tabs

    view <- UI.div #+ fmap element
            [ tabs'
            , content
            , navigation
            ]

    void $ UI.getBody win # set UI.children [view]


-- TODO this is no good
mkSessions :: Env -> Either String Sessions -> UI Element
mkSessions env  = \case 
    Left _ -> UI.p # set text "Session ikke valgt" -- TODO skulle ikke ku ske
    Right (Sessions sessions) -> do
        let children' = case sessions of
                (TreeZipper _ []) -> do
                    let children'' = mkSessions' env (Sessions sessions)
                    [children'']
                (TreeZipper x (Context _ f _:_)) -> do
                    let parent = mkParent env (Sessions sessions) f
                    let this = mkParent env (Sessions sessions) (datum x)
                    let children'' = mkSessions' env (Sessions sessions)
                    [parent, this, children'']

        UI.div #+ children'

mkSessions' :: Env -> Sessions -> UI Element
mkSessions' env (Sessions sessions) = do
        sessions' <- mapM (bimapM (mkDecision env (Sessions sessions)) (mkSession env (Sessions sessions))) elems
        UI.div #. "buttons has-addons" #+ fmap (element . fromEither . datum ) sessions'
            where
                tree = toRoseTree sessions --TODO dont call toRoseTree
                elems = RT.children tree --BØR KUNNNE EXTENDED så det virkerligsom med zippers men fuck.


mkParent :: Env -> Sessions -> Either Decisions Session -> UI Element
mkParent env sessions parent = do
    --TODO get rid of these extras by using extend
    elems <- fromEither <$> bimapM (mkDecision' env sessions) (mkSession' env sessions) parent
    UI.div #. "buttons has-addons" #+ [element elems]


mkDecision' :: Env -> Sessions -> Decisions -> UI Element
mkDecision' Env{..} (Sessions sessions) decision = do
    let name = show decision
    chooseButton <- mkButton "idd" name
    UI.on UI.click chooseButton $ \_ ->
        liftIO $ withMVar files $ \ Files{..} ->
            --TODO get rid of either by using extend
            mapM_ (writeSessions sessionsFile) (fmap Sessions (up sessions))
    return chooseButton


mkSession' :: Env -> Sessions -> Session -> UI Element
mkSession' Env{..} (Sessions sessions) session = do
    let name = show session
    chooseButton <- mkButton "idd" name
    UI.on UI.click chooseButton $ \_ ->
        liftIO $ withMVar files $ \ Files{..} ->
            --TODO get rid of either by using extend
            mapM_ (writeSessions sessionsFile) (fmap Sessions (up sessions))
    return chooseButton


mkDecision :: Env -> Sessions -> Decisions -> UI Element
mkDecision Env{..} (Sessions sessions) decision = do
    let name = show decision
    chooseButton <- mkButton "idd" name
    UI.on UI.click chooseButton $ \_ ->
        liftIO $ withMVar files $ \ Files{..} ->
            --TODO get rid of either by using extend
            mapM_ (writeSessions sessionsFile) (fmap Sessions (down (Left decision) sessions))
    return chooseButton


mkSession :: Env -> Sessions -> Session -> UI Element
mkSession Env{..} (Sessions sessions) session = do
    let name = show session
    chooseButton <- mkButton "idd" name
    UI.on UI.click chooseButton $ \_ ->
        liftIO $ withMVar files $ \ Files{..} ->
            --TODO get rid of either by using extend
            mapM_ (writeSessions sessionsFile) (fmap Sessions (down (Right session) sessions))
    return chooseButton
