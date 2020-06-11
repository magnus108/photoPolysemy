{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

module Lib.Location
    ( LocationFile(..)
    , getLocationFile
    , writeLocationFile
    , getLocationFile'
    , writeLocationFile'
    , initialState
    , Model(..)
    ) where


import Control.Concurrent
import Graphics.UI.Threepenny.Core

import Lib.Data
import Control.Lens

newtype LocationFile = LocationFile { unLocationFile :: FilePath }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


getLocationFile' :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String LocationFile)
getLocationFile' = readJSONFile'


writeLocationFile' :: (MonadIO m) => FilePath -> LocationFile -> m ()
writeLocationFile' = writeJSONFile


newtype Model = Model { unModel :: Data String LocationFile }


makeLenses ''Model


initialState :: Model
initialState = Model NotAsked


--TODO could do some notification on save..
write :: (MonadIO m, MonadThrow m) => MVar FilePath -> LocationFile -> m ()
write file locationFile = liftIO $ withMVar file $ \f -> writeLocationFile' f locationFile

--TODO could handle error on write.
writeLocationFile :: (MonadIO m) => MVar FilePath -> LocationFile -> m ThreadId
writeLocationFile file sessions = liftIO $ forkFinally (write file sessions) $ \ _ -> return ()


read :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String LocationFile)
read file = do
    liftIO $ withMVar file $ \f -> do
        getLocationFile' f


getLocationFile :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String LocationFile)
getLocationFile file = liftIO $ read file 
