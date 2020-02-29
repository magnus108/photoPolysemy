module Lib.Client.Session
    ( sessionsSection
    ) where


import Data.Bitraversable

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Tab
import Lib.Session
import Lib.Client.Tab

import Lib.App (Env(..),Files(..))
import Lib.Client.Element
import Control.Concurrent.MVar

import Utils.RoseTree as RT
import Utils.TreeZipper


sessionsSection :: Env -> Sessions -> Tabs -> UI Element
sessionsSection env@Env{..} sessions tabs = do

    content <- mkSessions env sessions

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    UI.div #+ fmap element
        [ tabs'
        , content
        , navigation
        ]


mkSessions :: Env -> Sessions -> UI Element
mkSessions env (Sessions sessions) = do
    sessions' <- mapM (bimapM (mkDecision env) (mkSession env (Sessions sessions))) elems
    UI.div #. "buttons has-addons" #+ fmap (element . fromEither . datum ) sessions'
        where
            tree = toRoseTree sessions --TODO dont call toRoseTree
            elems = RT.children tree

mkDecision :: Env -> Decisions -> UI Element
mkDecision _ decision = string (show decision)


mkSession :: Env -> Sessions -> Session -> UI Element
mkSession Env{..} (Sessions sessions) session = do
    let name = show session
    chooseButton <- mkButton "idd" name
    UI.on UI.click chooseButton $ \_ ->
        liftIO $ withMVar files $ \ Files{..} ->
            --TODO get rid of either
            mapM_ (writeSessions sessionsFile) (fmap Sessions (down (Right session) sessions))
    return chooseButton