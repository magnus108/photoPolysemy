module Lib.DagsdatoBackup
    ( DagsdatoBackup(..)
    , getDagsdatoBackup
    , writeDagsdatoBackup
    ) where

newtype DagsdatoBackup = DagsdatoBackup { unDagsdatoBackup :: FilePath }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


getDagsdatoBackup :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String DagsdatoBackup)
getDagsdatoBackup = readJSONFile'


writeDagsdatoBackup :: (MonadIO m) => FilePath -> DagsdatoBackup -> m ()
writeDagsdatoBackup = writeJSONFile
