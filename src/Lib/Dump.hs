module Lib.Dump
    ( Dump(..)
    , getDump
    ) where

newtype Dump = Dump { unDump :: FilePath }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


getDump :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String Dump)
getDump = readJSONFile'
