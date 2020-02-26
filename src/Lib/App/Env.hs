{-# LANGUAGE DeriveAnyClass #-}
module Lib.App.Env
       ( Env(..)
       , Files(..)
       , loadFiles
       ) where

data Files = Files
    { dumpFile :: !FilePath
    , doneshootingFile :: !FilePath
    , dagsdato :: !FilePath
    , shooting :: !FilePath
    , session :: !FilePath
    , photographersFile :: !FilePath
    , camera :: !FilePath
    , tabsFile :: !FilePath
    } deriving (Generic)
      deriving (FromJSON, ToJSON)
      deriving (Show)

loadFiles :: (MonadIO m, MonadThrow m) => FilePath -> m Files
loadFiles = readJSONFile

data Env = Env
    { files :: MVar Files
    }
