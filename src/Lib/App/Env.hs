{-# LANGUAGE DeriveAnyClass #-}
module Lib.App.Env
       ( Env(..)
       , Files(..)
       , loadFiles
       ) where

import Control.Concurrent.MVar

data Files = Files
    { dump :: !FilePath
    , doneshooting :: !FilePath
    , dagsdato :: !FilePath
    , shooting :: !FilePath
    , session :: !FilePath
    , photographer :: !FilePath
    , camera :: !FilePath
    } deriving (Generic)
      deriving (FromJSON, ToJSON)
      deriving (Show)

loadFiles :: (MonadIO m, MonadThrow m) => FilePath -> m Files
loadFiles = readJSONFile

data Env = Env
    { files :: MVar Files
    }
