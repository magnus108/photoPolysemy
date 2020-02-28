module Lib.Client.Session
    ( sessionsSection
    ) where


import Data.Bitraversable

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Tab
import Lib.Session
import Lib.Client.Tab

import Lib.App (Env(..))

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
    sessions' <- mapM (bimapM (mkDecision env) (mkSession env)) elems
    UI.div #. "buttons has-addons" #+ fmap (element . fromEither . datum ) sessions'
        where
            tree = toRoseTree sessions --TODO dont call toRoseTree
            elems = RT.children tree

mkDecision :: Env -> Decisions -> UI Element
mkDecision _ decision = string (show decision)


mkSession :: Env -> Session -> UI Element
mkSession _ session = UI.strong #+ [string (show session)]
