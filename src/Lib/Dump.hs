module Lib.Dump
    ( Dump(..)
    , DumpDir(..)
    , getDump
    , getDumpDir
    ) where

import System.Directory
import Control.Exception


newtype Dump = Dump { unDump :: FilePath }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


getDump :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String Dump)
getDump = readJSONFile'


newtype DumpDir = DumpDir { unDumpDir :: [FilePath] }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


getDumpDir :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String DumpDir)
getDumpDir filepath = do
    dir <- liftIO $ try (listDirectory filepath) <&> first (\e -> show (e :: SomeException))
    return $ DumpDir <$> dir
