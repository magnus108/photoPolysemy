{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

module Lib.Camera
    ( Cameras(..)
    , Camera(..)
    , getCameras
    , writeCameras
    ) where

import Utils.ListZipper

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


getCameras :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String Cameras)
getCameras = readJSONFile'


writeCameras :: (MonadIO m) => FilePath -> Cameras -> m ()
writeCameras = writeJSONFile
