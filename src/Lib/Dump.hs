{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Dump
    ( Dump(..)
    , DumpDir(..)
    , DumpModel(..)
    , dump
    , getDump
    , writeDump
    , getDump'
    , dumpDir
    , getDumpDir
    , Model(..)
    , initialStateTmp
    , initalState
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


getDump' :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String Dump)
getDump' = readJSONFile'


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


initialStateTmp :: Model
initialStateTmp = Model NotAsked NotAsked

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


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

newtype DumpModel = DumpModel { unModel :: Data String Dump }


initalState :: DumpModel
initalState = DumpModel NotAsked


writeDump' :: (MonadIO m) => FilePath -> Dump -> m ()
writeDump' = writeJSONFile


--TODO could do some notification on save..
write :: (MonadIO m, MonadThrow m) => MVar FilePath -> Dump -> m ()
write file dump' = liftIO $ withMVar file $ \f -> writeDump' f dump'


--TODO could handle error on write.
writeDump :: (MonadIO m) => MVar FilePath -> Dump -> m ThreadId
writeDump file dump' = liftIO $ forkFinally (write file dump') $ \ _ -> return ()


read :: (MonadIO m, MonadThrow m) => MVar FilePath -> Handler DumpModel -> m (Either String Dump)
read file handle = liftIO $ withMVar file $ \f -> do
        _ <- liftIO $ handle (DumpModel Loading)
        getDump' f


getDump :: (MonadIO m, MonadThrow m) => MVar FilePath -> Handler DumpModel -> m ThreadId
getDump file handle = liftIO $ forkFinally (read file handle) $ \case
    Left e -> handle $ DumpModel (Failure (show e))
    Right x -> case x of
            Left e' -> handle $ DumpModel (Failure e')
            Right s -> handle $ DumpModel (Data s)
