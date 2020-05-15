module Lib.Client.Session
    ( sessionsSection
    ) where


import Data.Bitraversable

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Utils.Comonad

import qualified Control.Lens as Lens

import Lib.App
import Lib.Translation
import Lib.Data
import Lib.Tab
import Lib.Client.Tab
import Lib.Session

import Lib.Client.Utils
import Lib.Client.Element


import qualified Utils.RoseTree as RT
import qualified Utils.TreeZipper as TZ


sessionsSection :: Env -> Window -> Translation -> Tabs -> Behavior Model -> UI ()
sessionsSection env@Env{..} win translation tabs bSessions = do
    let bView = mkSessions env translation <$> bSessions
    content <- UI.div #. "section" # sink item bView

    tabs' <- mkElement "nav" #. "section" #+ [mkTabs env tabs]
    navigation <- mkElement "footer" #. "section" #+ [mkNavigation env translation tabs]

    view <- UI.div #+ fmap element [ content ]

    void $ UI.getBody win # set children [tabs', view, navigation]


-- TODO this is no good
mkSessions :: Env -> Translation -> Model -> UI Element
mkSessions env@Env{..} translations model =
    case unModel model of
        NotAsked -> UI.p #+ [Lens.views starting string translations]
        Loading -> UI.p #+ [Lens.views loading string translations]
        Failure _ -> do
            err <- UI.p #+ [Lens.views sessionsError string translations]
            picker <- mkFilePicker "sessionPicker" (Lens.view filePicker translations) $ \file ->
                when (file /= "") $ do
                    --TODO er det engentligt det her man vil?
                    parseSessions <- liftIO $ getSessions' file
                    forM_ parseSessions $ writeSessions mSessionsFile

            UI.div # set children [err, picker]

        Data (Sessions sessions) -> do
            let children' = case sessions of
                    (TZ.TreeZipper _ []) -> do
                        let children'' = mkSessions' env translations (Sessions sessions)
                        [children'']

                    (TZ.TreeZipper (RT.Branch x xs) (TZ.Context _ f _:_)) -> do
                        let parent = mkParent env translations (Sessions sessions) f
                        let children'' = mkSessions' env translations (Sessions sessions)
                        [parent, children'']

                    (TZ.TreeZipper (RT.Leaf x) (TZ.Context _ f _:_)) -> do
                        let parent = mkParent env translations (Sessions sessions) f
                        let this = mkSelected env translations (Sessions sessions) x
                        [parent, this]

            UI.div #+ children'


mkSessions' :: Env -> Translation -> Sessions -> UI Element
mkSessions' env translations (Sessions sessions) = do
        sessions' <- mapM (bimapM (mkDecision env translations (Sessions sessions)) (mkSession env translations (Sessions sessions))) elems
        UI.div #. "buttons has-addons" #+ fmap (element . fromEither . RT.datum ) sessions'
            where
                tree = TZ.toRoseTree sessions --TODO dont call toRoseTree
                elems = RT.children tree --BØR KUNNNE EXTENDED så det virkerligsom med zippers men fuck.


mkSelected :: Env -> Translation -> Sessions -> Session -> UI Element
mkSelected Env{..} translations (Sessions sessions) session = do
    let name = translationSession session translations
    mkButton "idd" name #. "button is-selected" # set (attr "disabled") "true"


--TODO: giver ikke mening b'r kun kunne tage en decision
mkParent :: Env -> Translation -> Sessions -> Either Decisions Session -> UI Element
mkParent env translations sessions parent = do
    --TODO get rid of these extras by using extend
    elems <- fromEither <$> bimapM (mkDecision' env translations sessions) (mkSession' env translations sessions) parent
    UI.div #. "buttons has-addons" #+ [element elems]


mkDecision' :: Env -> Translation -> Sessions -> Decisions -> UI Element
mkDecision' Env{..} translations (Sessions sessions) decision = do
    let name = Lens.view up translations
    chooseButton <- mkButton "idd" name
    UI.on UI.click chooseButton $ \_ ->
            --TODO get rid of either by using extend
            forM_ (fmap Sessions (TZ.up sessions)) $ writeSessions mSessionsFile
    return chooseButton


mkSession' :: Env -> Translation -> Sessions -> Session -> UI Element
mkSession' Env{..} translations (Sessions sessions) session = do
    let name = translationSession session translations
    chooseButton <- mkButton "idd" name
    UI.on UI.click chooseButton $ \_ ->
            forM_ (fmap Sessions (TZ.up sessions)) $ writeSessions mSessionsFile
    return chooseButton


mkDecision :: Env -> Translation -> Sessions -> Decisions -> UI Element
mkDecision Env{..} translations (Sessions sessions) decision = do
    let name = translationDecision decision translations
    chooseButton <- mkButton "idd" name
    UI.on UI.click chooseButton $ \_ ->
            forM_ (fmap Sessions (TZ.down (Left decision) sessions)) $ writeSessions mSessionsFile
    return chooseButton


mkSession :: Env -> Translation -> Sessions -> Session -> UI Element
mkSession Env{..} translations (Sessions sessions) session = do
    let name = translationSession session translations
    chooseButton <- mkButton "idd" name
    UI.on UI.click chooseButton $ \_ ->
            --TODO get rid of either by using extend
            forM_ (fmap Sessions (TZ.down (Right session) sessions)) $ writeSessions mSessionsFile
    return chooseButton
