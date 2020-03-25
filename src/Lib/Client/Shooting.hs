module Lib.Client.Shooting
    ( shootingsSection
    ) where


import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Tab
import Lib.Shooting
import Lib.Client.Tab

import Lib.App (Env(..), Files(..))

import Lib.Client.Utils
import Lib.Client.Element
import Utils.Comonad
import Utils.ListZipper (focus)
import qualified Utils.ListZipper as ListZipper

import Control.Concurrent.MVar


shootingsSection :: Env -> Window -> Event (Either String Shootings) -> Tabs -> UI ()
shootingsSection env@Env{..} win eShootings tabs = do
    shootings <- liftIO $ withMVar files $ \ Files{..} -> getShootings shootingsFile
    bShootings <- stepper shootings eShootings

    content <- UI.div # sink item (mkShootings env <$> bShootings)

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    view <- UI.div #+ fmap element
        [ tabs'
        , content
        , navigation
        ]

    void $ UI.getBody win # set children [view]


mkShootings :: Env -> Either String Shootings -> UI Element
mkShootings env = \case
    Left _ -> UI.div # set text "Shooting ikke sat" -- TODO skulle ikke se
    Right (Shootings shootings) -> do
        let currentShooting = focus shootings
        let elems = shootings =>> \shootings'' -> let
                    thisShooting = focus shootings''
                in
                    ( thisShooting
                    , thisShooting == currentShooting
                    , Shootings shootings''
                    )
        shootings' <- mapM (mkShooting env) elems
        UI.div #. "buttons has-addons" # set children (ListZipper.toList shootings')



mkShooting :: Env -> (Shooting, Bool, Shootings) -> UI Element
mkShooting Env{..} (shooting, isCenter, shootings)
    | isCenter = do
        let name = show shooting
        mkButton "idd" name #. "button is-selected" # set (attr "disabled") "true"
    | otherwise = do
        let name = show shooting
        forwardButton <- mkButton "idd" name
        UI.on UI.click forwardButton $ \_ ->
            liftIO $ withMVar files $ \ Files{..} ->
                writeShootings shootingsFile shootings
        return forwardButton
