{-# LANGUAGE DeriveAnyClass #-}
module Lib.App.Env
       ( Env(..)
       , Files(..)
       , loadFiles
       ) where

import Lib.App.Files2

data Files = Files
    { shootingsFile :: !FilePath
    , sessionsFile :: !FilePath
    , gradesFile :: !FilePath
    , camerasFile :: !FilePath
    , tabsFile :: !FilePath
    , locationConfigFile :: !FilePath
    } deriving (Generic)
      deriving (FromJSON, ToJSON)
      deriving (Show)

data Env = Env
    { files :: MVar Files
    , mPhotographersFile :: MVar FilePath
    , mGradesFile :: MVar FilePath
    , mDumpFile :: MVar FilePath
    , mDagsdatoFile :: MVar FilePath
    , mDagsdatoBackupFile :: MVar FilePath
    , mDoneshootingFile :: MVar FilePath
    , mTranslationFile :: MVar FilePath
    }
