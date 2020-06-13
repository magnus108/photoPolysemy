{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Dump
    ( Dump(..)
    , DumpDir(..)
    , DumpModel(..)
    , DumpDirModel(..)
    , count
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
import Control.Exception

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


read :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Dump)
read file = liftIO $ withMVar file $ getDump'


getDump :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Dump)
getDump file = liftIO $ read file

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------



newtype DumpDir = DumpDir { unDumpDir :: [FilePath] }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


count :: DumpDir -> Int
count = length . unDumpDir


getDumpFiles :: Dump -> Camera.Camera -> IO (Either String DumpDir)
getDumpFiles dump camera = do
    let filepath = unDump dump
    files <- try $ listDirectory filepath :: IO (Either SomeException [FilePath])
    case files of
        Left _ -> return $ Left "problem reading dump"
        Right filess -> do
                    traceShowM "filess"
                    traceShowM filess
                    validateDump <- mapM (\file ->  do
                            "traceShowM file"
                            traceShowM file
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
                        ) filess

                    let crs = filter (\x -> isExtensionOf (snd (Camera.toExtension camera)) x || isExtensionOf (fst (Camera.toExtension camera)) x) filess

                    if and validateDump then
                        return $ Right $ DumpDir crs
                    else 
                        return $ Left "Der er fejl med filer i dump"


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


getDumpDir :: (MonadIO m, MonadThrow m) => MVar FilePath -> MVar FilePath -> m (Either String DumpDir)
getDumpDir file mCamerasFile = liftIO $ readDir file mCamerasFile
