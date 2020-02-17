{-# LANGUAGE DeriveAnyClass #-}

module Lib.Dump
    ( Dump(..)
    , get
    ) where

data Dump = Dump FilePath
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


getDump :: (MonadIO m, MonadThrow m) => FilePath -> m Dump
getDump = readJSONFile
