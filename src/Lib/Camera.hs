{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Camera
    ( Cameras(..)
    , toExtension
    , Camera(..)
    , read
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


toExtension :: Camera -> (String, String)
toExtension CR2 = ("CR2", "cr2")
toExtension CR3 = ("CR3", "cr3")

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


read :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Cameras)
read file = liftIO $ withMVar file $ \f -> getCameras' f


getCameras :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Cameras)
getCameras file = liftIO $ read file
