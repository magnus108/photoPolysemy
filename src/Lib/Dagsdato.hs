{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Dagsdato
    ( Dagsdato(..)
    , getDagsdato
    , writeDagsdato
    ) where

newtype Dagsdato = Dagsdato { unDagsdato :: FilePath }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


getDagsdato :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String Dagsdato)
getDagsdato = readJSONFile'


writeDagsdato :: (MonadIO m) => FilePath -> Dagsdato -> m ()
writeDagsdato = writeJSONFile
