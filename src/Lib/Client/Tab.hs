module Lib.Client.Tab
    ( mkTabs
    , mkTab
    , next
    , prev
    ) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Tab

import Lib.Client.Element
import Utils.Comonad
import Utils.ListZipper (focus, backward, forward)
import qualified Utils.ListZipper as ListZipper
import Control.Concurrent.MVar
import Lib.App (Env(..), Files(..))


mkTabs :: Env -> Tabs -> UI Element
mkTabs env (Tabs tabs) = do
    tabs' <- mapM (mkTab env) elems
    UI.div #+ fmap element (ListZipper.toList (fmap fst tabs'))
        where 
            --TODO Lav en indexedmap for zippers
            currentTab = focus tabs
            elems = tabs =>> \tabs'' ->
                let
                    thisTab = focus tabs''
                in
                    (thisTab, thisTab == currentTab, Tabs tabs'')


mkTab :: Env -> (Tab, Bool, Tabs) -> UI (Element, Element)
mkTab Env{..} (tab, isCenter, tabs)
    | isCenter = do
        bu <- UI.string (show tab)
        vi <- UI.strong #+ [element bu]
        return (bu, vi)
    | otherwise = do
        let name = show tab
        (forwardButton, forwardView) <- mkButton "idd" name
        UI.on UI.click forwardButton $ \_ -> 
            liftIO $ withMVar files $ \ Files{..} ->
                writeTabs tabsFile tabs
        return (forwardButton, forwardView)


prev :: Env -> Tabs -> UI (Maybe (Element, Element))
prev Env{..} tabs =
    control (ListZipper.isLeft, "prev","prev") (unTabs tabs) $ \ _ ->
        liftIO $ withMVar files $ \ Files{..} ->
            writeTabs tabsFile (Tabs (backward (unTabs tabs)))


next :: Env -> Tabs -> UI (Maybe (Element, Element))
next Env{..} tabs =
    control (ListZipper.isRight,"next","next") (unTabs tabs) $ \ _ ->
        liftIO $ withMVar files $ \ Files{..} ->
            writeTabs tabsFile (Tabs (forward (unTabs tabs)))
