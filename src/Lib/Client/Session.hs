module Lib.Client.Session
    ( sessionsSection
    ) where


import Data.Bitraversable

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Tab
import Lib.Session
import Lib.Client.Tab

import Lib.Client.Utils
import Lib.App (Env(..),Files(..))
import Lib.Client.Element
import Control.Concurrent.MVar

import Utils.RoseTree as RT
import Utils.TreeZipper


sessionsSection :: Env -> Window -> Behavior Sessions -> Tabs -> UI ()
sessionsSection env@Env{..} win bSessions tabs = do

    let content = mkSessions env bSessions

    let tabs' = mkTabs env tabs
    let navigation = mkNavigation env tabs

    view <- UI.div # sink items
        (sequenceA
            [ pure tabs'
            , content
            , pure navigation
            ])

    void $ UI.getBody win #+ fmap element [view]


mkSessions :: Env -> Behavior Sessions -> Behavior (UI Element)
mkSessions env bSessions = bSessions <&> \(Sessions sessions) -> case sessions of
        (TreeZipper _ []) -> mkSessions' env (Sessions sessions)
        (TreeZipper _ (Context _ f _:_)) -> do
            children' <- mkSessions' env (Sessions sessions)
            parent <- mkParent env (Sessions sessions) f
            UI.div #+ fmap element [parent, children']


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
