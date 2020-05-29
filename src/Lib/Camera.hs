{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Camera
    ( Cameras(..)
    , Camera(..)
    , getCameras'
    , writeCameras'
    , getCameras
    , writeCameras
    , Model(..)
    , initalState
    ) where

import Utils.ListZipper

import Control.Concurrent
import Graphics.UI.Threepenny.Core

import Lib.Data

import Control.Lens

data Camera
    = CR2
    | CR3
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


newtype Cameras = Cameras { unCameras :: ListZipper Camera }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


getCameras' :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String Cameras)
getCameras' = readJSONFile'


writeCameras' :: (MonadIO m) => FilePath -> Cameras -> m ()
writeCameras' = writeJSONFile


newtype Model = Model { unModel :: Data String Cameras }


makeLenses ''Model


initalState :: Model
initalState = Model NotAsked


--TODO could do some notification on save..
write :: (MonadIO m, MonadThrow m) => MVar FilePath -> Cameras -> m ()
write file cameras = liftIO $ withMVar file $ \f -> writeCameras' f cameras

--TODO could handle error on write.
writeCameras :: (MonadIO m) => MVar FilePath -> Cameras -> m ThreadId
writeCameras file cameras = liftIO $ forkFinally (write file cameras ) $ \ _ -> return ()


read :: (MonadIO m, MonadThrow m) => MVar FilePath -> Handler Model -> m (Either String Cameras)
read file _ = liftIO $ withMVar file $ \f -> do
        --_ <- liftIO $ handle (Model Loading)
        getCameras' f


getCameras :: (MonadIO m, MonadThrow m) => MVar FilePath -> Handler Model -> m ()
getCameras file handle = liftIO $ (read file handle) >>= \case
            Left e' -> handle $ Model (Failure e')
            Right s -> handle $ Model (Data s)
