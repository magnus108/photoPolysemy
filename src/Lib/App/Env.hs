{-# LANGUAGE DeriveAnyClass #-}
module Lib.App.Env
       ( Env(..)
       , Action(..)
       ) where


import qualified Control.Concurrent.Chan as Chan
import qualified Lib.Photographer as Photographer


data Action
    = ReadPhographers
    | WritePhographers Photographer.Photographers
        deriving Show

data Env = Env
    { serverRoot :: FilePath
    , chan :: Chan.Chan Action
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
    , mTabsFile :: MVar FilePath
    }
