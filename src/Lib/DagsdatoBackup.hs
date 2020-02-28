{-# LANGUAGE DeriveAnyClass #-}

module Lib.DagsdatoBackup
    ( DagsdatoBackup(..)
    , getDagsdatoBackup
    , writeDagsdatoBackup
    ) where

data DagsdatoBackup = DagsdatoBackup FilePath
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


getDagsdatoBackup :: (MonadIO m, MonadThrow m) => FilePath -> m DagsdatoBackup
getDagsdatoBackup = readJSONFile


writeDagsdatoBackup :: (MonadIO m) => FilePath -> DagsdatoBackup -> m ()
writeDagsdatoBackup = writeJSONFile
