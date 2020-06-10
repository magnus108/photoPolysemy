{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
module Lib.Build
    ( Build(..)
    , Model(..)
    , write
    , getBuild
    , toString
    , initalState
    ) where

import Prelude hiding (toString)
import qualified Lib.Translation  as Translation --todo should not be here

import qualified Data.List.Index
import Control.Concurrent
import Data.Strings hiding (toString)
import Graphics.UI.Threepenny.Core

import Control.Lens
import Utils.Comonad

import qualified Control.Lens as Lens

import Lib.Data
import qualified Lib.Shooting as Shooting
import qualified Lib.Photographer as Photographer
import qualified Lib.Photographee as Photographee
import qualified Lib.Session as Session
import qualified Lib.Location as Location
import qualified Lib.Grade as Grade
import qualified Lib.Dump as Dump
import qualified Lib.Doneshooting as Doneshooting
import qualified Lib.Dagsdato as Dagsdato
import qualified Lib.DagsdatoBackup as DagsdatoBackup
import qualified Lib.Camera as Camera

import Development.Shake
import Development.Shake.FilePath

import Data.Time.Format
import Data.Time.Clock


data Build
    = DoneBuild Photographee.Photographee String
    | Building Photographee.Photographee String
    | NoBuild
    deriving (Eq, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


toString :: Build -> Translation.Translation -> String
toString build translation = Lens.view translator translation ++ ". " ++ info
    where 
        translator = case build of
                (DoneBuild photographee x) -> Translation.doneBuild
                (Building photographee x) -> Translation.building
                NoBuild -> Translation.noBuild
        info = case build of
                (DoneBuild photographee x) -> Lens.view Photographee.name photographee ++ " " ++ x
                (Building photographee x) -> Lens.view Photographee.name photographee ++ " " ++ x
                NoBuild -> ""


getBuild' :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String Build)
getBuild' = readJSONFile'


writeBuild' :: (MonadIO m) => FilePath -> Build -> m ()
writeBuild' = writeJSONFile

newtype Model = Model { unModel :: Data String Build }

makeLenses ''Model


initalState :: Model
initalState = Model NotAsked


--TODO could do some notification on save..
write :: (MonadIO m, MonadThrow m) => MVar FilePath -> Build -> m ()
write file build = liftIO $ withMVar file $ \f -> writeBuild' f build

--TODO could handle error on write.
writeBuild :: (MonadIO m) => MVar FilePath -> Build -> m ThreadId
writeBuild file build = liftIO $ forkFinally (write file build ) $ \ _ -> return ()


read :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Build)
read file = liftIO $ withMVar file $ \f -> getBuild' f


getBuild :: (MonadIO m, MonadThrow m) => MVar FilePath -> Handler Model -> m ()
getBuild file handle = liftIO $ (read file) >>= \case
            Left e' -> handle $ Model (Failure "Kunne ikke finde byg")
            Right s -> handle $ Model (Data s)
