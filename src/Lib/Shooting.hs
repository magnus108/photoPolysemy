{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

module Lib.Shooting
    ( Shootings(..)
    , Shooting(..)
    , getShootings
    , writeShootings
    ) where

import Utils.ListZipper

data Shooting
    = ReShoot
    | Normal
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


newtype Shootings = Shootings { unShootings :: ListZipper Shooting }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


getShootings :: (MonadIO m, MonadThrow m) => FilePath -> m Shootings
getShootings = readJSONFile


writeShootings :: (MonadIO m) => FilePath -> Shootings -> m ()
writeShootings = writeJSONFile
