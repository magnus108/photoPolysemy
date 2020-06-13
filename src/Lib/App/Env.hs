{-# LANGUAGE DeriveAnyClass #-}
module Lib.App.Env
       ( Env(..)
       ) where

data Env = Env
    { mLocationConfigFile :: MVar FilePath
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
    , mTabsFile :: MVar FilePath
    }
