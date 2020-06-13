{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Doneshooting
    ( Doneshooting(..)
    , count
    , DoneshootingDir(..)
    , DoneshootingDirModel(..)
    , getDoneshootingDir
    , initialStateDir
    , Model(..)
    , getDoneshooting'
    , getDoneshooting
    , writeDoneshooting
    , initialState
    ) where


import Control.Exception
import Control.Concurrent

import Lib.Data
import Utils.Comonad
import System.FilePath
import System.Directory

import qualified Lib.Camera as Camera

import Control.Lens

newtype Doneshooting = Doneshooting { unDoneshooting :: FilePath }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


newtype Model = Model { unModel :: Data String Doneshooting }

makeLenses ''Model

initialState :: Model
initialState = Model NotAsked

getDoneshooting' :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String Doneshooting)
getDoneshooting' = readJSONFile'


writeDoneshooting' :: (MonadIO m) => FilePath -> Doneshooting -> m ()
writeDoneshooting' = writeJSONFile


--TODO could do some notification on save..
write :: (MonadIO m, MonadThrow m) => MVar FilePath -> Doneshooting -> m ()
write file dagsdato' = liftIO $ withMVar file $ \f -> writeDoneshooting' f dagsdato'


--TODO could handle error on write.
writeDoneshooting :: (MonadIO m) => MVar FilePath -> Doneshooting -> m ThreadId
writeDoneshooting file dagsdato' = liftIO $ forkFinally (write file dagsdato') $ \ _ -> return ()


read :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Doneshooting)
read file = liftIO $ withMVar file $ \f -> do
        file' <- getDoneshooting' f
        case file' of
          Left e -> return $ Left e
          Right string -> do
                isDir <- doesDirectoryExist (unDoneshooting string)
                if isDir then
                    return $ Right string
                else
                    return $ Left "Er ikke mappe"
            


getDoneshooting :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Doneshooting)
getDoneshooting file = liftIO $ read file

--
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------



newtype DoneshootingDir = DoneshootingDir { unDoneshootingDir :: [FilePath] }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)



getDoneshootingFiles :: Doneshooting -> Camera.Camera -> IO (Either String DoneshootingDir)
getDoneshootingFiles doneshooting camera = do
    let filepath = unDoneshooting doneshooting
    files <- try $ listDirectory filepath :: IO (Either SomeException [FilePath])
    case files of
      Left _ -> return $ Left "could not read"
      Right files' ->
          return $ Right $ DoneshootingDir files'


getDoneshootingDir' :: (MonadIO m, MonadThrow m) => Doneshooting -> Camera.Camera -> m (Either String DoneshootingDir)
getDoneshootingDir' doneshooting camera = do
    dir <- liftIO $ getDoneshootingFiles doneshooting camera
    return $ dir


newtype DoneshootingDirModel = DoneshootingDirModel { unDoneshootingDirModel :: Data String DoneshootingDir }

count :: DoneshootingDir -> Int
count = length . unDoneshootingDir


initialStateDir :: DoneshootingDirModel
initialStateDir = DoneshootingDirModel NotAsked


readDir :: (MonadIO m, MonadThrow m) => MVar FilePath -> MVar FilePath -> m (Either String DoneshootingDir)
readDir file mCamerasFile =
    liftIO $ withMVar file $ \f -> do
        cameras <- Camera.read mCamerasFile
        case cameras of
                Left x -> return $ Left x
                Right cameras' -> do
                    doneshootingPath <- getDoneshooting' f --TODO fix this shit
                    case doneshootingPath of
                            Left x -> return $ Left x
                            Right ff -> getDoneshootingDir' ff (extract (Camera.unCameras cameras'))


getDoneshootingDir :: (MonadIO m, MonadThrow m) => MVar FilePath -> MVar FilePath -> m (Either String DoneshootingDir)
getDoneshootingDir file mCamerasFile = liftIO $ readDir file mCamerasFile
