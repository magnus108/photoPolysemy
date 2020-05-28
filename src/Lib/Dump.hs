{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Dump
    ( Dump(..)
    , DumpDir(..)
    , DumpModel(..)
    , DumpDirModel(..)
    , getDump
    , writeDump
    , getDump'
    , getDumpDir
    , initalState
    , initalStateDir
    ) where

import System.Directory
import Control.Exception (try)
import Control.Concurrent (ThreadId, withMVar, forkFinally)
import Graphics.UI.Threepenny.Core

import Lib.Data

import Control.Lens

newtype Dump = Dump { unDump :: FilePath }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


getDump' :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String Dump)
getDump' = readJSONFile'


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

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------



newtype DumpDir = DumpDir { unDumpDir :: [FilePath] }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


getDumpDir' :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String DumpDir)
getDumpDir' filepath = do
    dir <- liftIO $ try (listDirectory filepath) <&> first (\e -> show (e :: SomeException))
    return $ DumpDir <$> dir


newtype DumpDirModel = DumpDirModel { unDumpDirModel :: Data String DumpDir }


initalStateDir :: DumpDirModel
initalStateDir = DumpDirModel NotAsked


readDir :: (MonadIO m, MonadThrow m) => MVar FilePath -> Handler DumpDirModel -> m (Either String DumpDir)
readDir file handle = liftIO $ withMVar file $ \f -> do
--        _ <- liftIO $ handle (DumpDirModel Loading)
        dumpPath <- getDump' f --TODO fix this shit
        case dumpPath of
            Left x -> return $ Left x
            Right ff -> getDumpDir' (unDump ff)


getDumpDir :: (MonadIO m, MonadThrow m) => MVar FilePath -> Handler DumpDirModel -> m ()
getDumpDir file handle = liftIO $ (readDir file handle) >>= \case
                Left e' -> handle $ DumpDirModel (Failure e')
                Right s -> handle $ DumpDirModel (Data s)
