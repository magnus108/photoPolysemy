{-# LANGUAGE DeriveAnyClass #-}

module Lib.Dagsdato
    ( Dagsdato(..)
    , getDagsdato
    , writeDagsdato
    ) where

data Dagsdato = Dagsdato FilePath
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


getDagsdato :: (MonadIO m, MonadThrow m) => FilePath -> m Dagsdato
getDagsdato = readJSONFile


writeDagsdato :: (MonadIO m) => FilePath -> Dagsdato -> m ()
writeDagsdato = writeJSONFile
