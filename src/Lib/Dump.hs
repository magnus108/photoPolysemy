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

import System.FilePath
import System.Directory
import Control.Concurrent (ThreadId, withMVar, forkFinally)
import Graphics.UI.Threepenny.Core

import Utils.Comonad
import Lib.Data
import qualified Lib.Camera as Camera


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
read file _ = liftIO $ withMVar file $ \f -> do
        getDump' f


getDump :: (MonadIO m, MonadThrow m) => MVar FilePath -> Handler DumpModel -> m ()
getDump file handle = liftIO $ (read file handle) >>= \case
            Left e' -> handle $ DumpModel (Failure e')
            Right s -> handle $ DumpModel (Data s)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------



newtype DumpDir = DumpDir { unDumpDir :: [FilePath] }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


getDumpFiles :: Dump -> Camera.Camera -> IO (Either String DumpDir)
getDumpFiles dump camera = do
    let filepath = unDump dump
    files <- listDirectory filepath
    validateDump <- mapM (\file ->  do
            if or [ isExtensionOf (fst (Camera.toExtension camera)) file
                  , isExtensionOf (snd (Camera.toExtension camera)) file
                  ] then
                    doesFileExist (filepath </> file -<.> "jpg") ||^ (doesFileExist (filepath </> file -<.> "JPG"))
            else if or [ isExtensionOf "JPG" file
                       , isExtensionOf "jpg" file
                       ] then
                            doesFileExist (filepath </> file -<.> (fst (Camera.toExtension camera)))
                            ||^ (doesFileExist (filepath </> file -<.> (snd (Camera.toExtension camera))))
            else
               return False
        ) files

    let crs = filter (\x -> isExtensionOf (snd (Camera.toExtension camera)) x || isExtensionOf (fst (Camera.toExtension camera)) x) files

    if and validateDump then
        return $ Right $ DumpDir crs
    else 
        return $ Left "Dumpfolder"


getDumpDir' :: (MonadIO m, MonadThrow m) => Dump -> Camera.Camera -> m (Either String DumpDir)
getDumpDir' dump camera = do
    dir <- liftIO $ getDumpFiles dump camera
    return $ dir


newtype DumpDirModel = DumpDirModel { unDumpDirModel :: Data String DumpDir }


initalStateDir :: DumpDirModel
initalStateDir = DumpDirModel NotAsked


readDir :: (MonadIO m, MonadThrow m) => MVar FilePath -> MVar FilePath -> m (Either String DumpDir)
readDir file mCamerasFile =
    liftIO $ withMVar file $ \f -> do
        cameras <- Camera.read mCamerasFile
        case cameras of
                Left x -> return $ Left x
                Right cameras' -> do
                    dumpPath <- getDump' f --TODO fix this shit
                    case dumpPath of
                            Left x -> return $ Left x
                            Right ff -> getDumpDir' ff (extract (Camera.unCameras cameras'))


getDumpDir :: (MonadIO m, MonadThrow m) => MVar FilePath -> MVar FilePath -> Handler DumpDirModel -> m ()
getDumpDir file mCamerasFile handle = liftIO $ (readDir file mCamerasFile) >>= \case
                Left e' -> handle $ DumpDirModel (Failure e')
                Right s -> handle $ DumpDirModel (Data s)
