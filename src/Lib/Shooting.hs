{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

module Lib.Shooting
    ( Shootings(..)
    , Shooting(..)
    , toInteger
    , getShootings
    , writeShootings
    , getShootings'
    , writeShootings'
    , Model(..)
    , initialState
    ) where

import Prelude hiding (toInteger)

import Control.Concurrent

import Lib.Data

import Control.Lens

import Utils.ListZipper

data Shooting
    = ReShoot
    | Normal
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)

toInteger :: Shooting -> Integer
toInteger = \case
    Normal -> 1
    ReShoot -> 2

newtype Shootings = Shootings { unShootings :: ListZipper Shooting }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


getShootings' :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String Shootings)
getShootings' = readJSONFile'


writeShootings' :: (MonadIO m) => FilePath -> Shootings -> m ()
writeShootings' = writeJSONFile


newtype Model = Model { unModel :: Data String Shootings }


makeLenses ''Model


initialState :: Model
initialState = Model NotAsked


--TODO could do some notification on save..
write :: (MonadIO m, MonadThrow m) => MVar FilePath -> Shootings -> m ()
write file shootings = liftIO $ withMVar file $ \f -> writeShootings' f shootings

--TODO could handle error on write.
writeShootings :: (MonadIO m) => MVar FilePath -> Shootings -> m ThreadId
writeShootings file shootings = liftIO $ forkFinally (write file shootings) $ \ _ -> return ()


read :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Shootings)
read file = liftIO $ withMVar file $ \f -> do
        getShootings' f


getShootings :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Shootings)
getShootings file = liftIO $ read file
