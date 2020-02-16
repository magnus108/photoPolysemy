{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Doneshooting
    ( Doneshooting(..)
    , getDoneshooting
    ) where

import Control.Lens

data Doneshooting
    = YesDoneshooting FilePath FilePath
    | NoDoneshooting
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)

makeLenses ''Doneshooting


getDoneshooting :: (MonadIO m, MonadThrow m) => FilePath -> m Doneshooting
getDoneshooting filepath = liftIO
           $ runConduitRes
           $ sourceFileBS filepath
           .| sinkFromJSON
