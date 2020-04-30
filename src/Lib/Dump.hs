{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Dump
    ( Dump(..)
    , DumpDir(..)
    , dump
    , getDump
    , dumpDir
    , getDumpDir
    , Model(..)
    , initialState
    ) where

import System.Directory
import Control.Exception (try)
import Control.Concurrent
import Graphics.UI.Threepenny.Core

import Lib.Data

import Control.Lens

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


getDumpDir' :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String DumpDir)
getDumpDir' filepath = do
    dir <- liftIO $ try (listDirectory filepath) <&> first (\e -> show (e :: SomeException))
    return $ DumpDir <$> dir


data Model = Model { _dumpDir :: Data String DumpDir
                   , _dump :: Data String Dump
                   }

makeLenses ''Model


initialState :: Model
initialState = Model NotAsked NotAsked

forker :: (MonadIO m, MonadThrow m) => FilePath -> Handler (Data String DumpDir) -> m (Either String DumpDir)
forker file handle = do
    _ <- liftIO $ handle Loading
    getDumpDir' file


getDumpDir :: (MonadIO m, MonadThrow m) => FilePath -> Handler (Data String DumpDir) -> m ThreadId
getDumpDir file handle = do
    liftIO $ forkFinally (forker file handle) $ \res -> do
        case res of
            Left e -> handle $ Failure (show e)
            Right x -> case x of
                    Left e' -> handle $ Failure e'
                    Right s -> handle $ Data s
