{-# LANGUAGE DeriveAnyClass #-}
module Lib.App.Env
       ( Env(..)
       , Files(..)
       ) where

data Files = Files
    { tabsFile :: !FilePath
    } deriving (Generic)
      deriving (FromJSON, ToJSON)
      deriving (Show)

data Env = Env
    { files :: MVar Files
    , mLocationConfigFile :: MVar FilePath
    , mPhotographersFile :: MVar FilePath
    , mSessionsFile :: MVar FilePath
    , mGradesFile :: MVar FilePath
    , mCamerasFile :: MVar FilePath
    , mDumpFile :: MVar FilePath
    , mDagsdatoFile :: MVar FilePath
    , mDagsdatoBackupFile :: MVar FilePath
    , mDoneshootingFile :: MVar FilePath
    , mTranslationFile :: MVar FilePath
    , mShootingsFile :: MVar FilePath
    , mPhotographeesFile :: MVar FilePath
    , mBuildFile :: MVar FilePath
    }
