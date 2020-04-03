{-# LANGUAGE DeriveAnyClass #-}
module Lib.App.Files2
       ( Files2(..)
       , loadFiles
       ) where

data Files2 = Files2
    { dumpFile :: !FilePath
    , doneshootingFile :: !FilePath
    , dagsdatoFile :: !FilePath
    , dagsdatoBackupFile :: !FilePath
    , shootingsFile :: !FilePath
    , sessionsFile :: !FilePath
    , photographersFile :: !FilePath
    , gradesFile :: !FilePath
    , camerasFile :: !FilePath
    , tabsFile :: !FilePath
    , locationConfigFile :: !FilePath
    } deriving (Generic)
      deriving (FromJSON, ToJSON)
      deriving (Show)

loadFiles :: (MonadIO m, MonadThrow m) => FilePath -> m Files2
loadFiles = readJSONFile
