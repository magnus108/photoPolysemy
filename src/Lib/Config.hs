{-# LANGUAGE DeriveAnyClass #-}

module Lib.Config
       ( Config (..)
       , loadConfig
       ) where

data Config = Config
    { cDump :: !FilePath
    , cDoneshooting :: !FilePath
    , cDagsdato :: !FilePath
    , cShooting :: !FilePath
    , cSession :: !FilePath
    , cPhotographer :: !FilePath
    , cCamera :: !FilePath
    } deriving (Generic)
      deriving (FromJSON, ToJSON)
      deriving (Show)


loadConfig :: (MonadIO m, MonadThrow m) => FilePath -> m Config
loadConfig filepath = liftIO
           $ runConduitRes
           $ sourceFileBS filepath
           .| sinkFromJSON
