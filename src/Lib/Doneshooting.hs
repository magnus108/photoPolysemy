{-# LANGUAGE DeriveAnyClass #-}

module Lib.Doneshooting
    ( Doneshooting(..)
    , getDoneshooting
    ) where

data Doneshooting = Doneshooting FilePath
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


getDoneshooting :: (MonadIO m, MonadThrow m) => FilePath -> m Doneshooting
getDoneshooting = readJSONFile
