{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib.Doneshooting
    ( Doneshooting(..)
    , getDoneshooting
    , writeDoneshooting
    ) where

newtype Doneshooting = Doneshooting { unDoneshooting :: FilePath }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


getDoneshooting :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String Doneshooting)
getDoneshooting = readJSONFile'


writeDoneshooting :: (MonadIO m) => FilePath -> Doneshooting -> m ()
writeDoneshooting = writeJSONFile
