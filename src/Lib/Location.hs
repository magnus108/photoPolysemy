{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

module Lib.Location
    ( LocationFile(..)
    , getLocationFile
    , writeLocationFile
    ) where

newtype LocationFile = LocationFile { unLocationFile :: FilePath }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


getLocationFile :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String LocationFile)
getLocationFile = readJSONFile'


writeLocationFile :: (MonadIO m) => FilePath -> LocationFile -> m ()
writeLocationFile = writeJSONFile
