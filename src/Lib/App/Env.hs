{-# LANGUAGE DeriveAnyClass #-}
module Lib.App.Env
       ( Env(..)
       , Files(..)
       , loadFiles
       ) where

data Files = Files
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

loadFiles :: (MonadIO m, MonadThrow m) => FilePath -> m Files
loadFiles = readJSONFile

data Env = Env
    { files :: MVar Files
    }
