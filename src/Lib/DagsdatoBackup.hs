{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.DagsdatoBackup
    ( DagsdatoBackup(..)
    , getDagsdatoBackup
    , getDagsdatoBackup'
    , writeDagsdatoBackup
    , Model(..)
    , initialState
    ) where


import Control.Concurrent
import Graphics.UI.Threepenny.Core

import Lib.Data

import Control.Lens

newtype DagsdatoBackup = DagsdatoBackup { unDagsdatoBackup :: FilePath }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)

newtype Model = Model { unModel :: Data String DagsdatoBackup }

makeLenses ''Model

initialState :: Model
initialState = Model NotAsked

getDagsdatoBackup' :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String DagsdatoBackup)
getDagsdatoBackup' = readJSONFile'


writeDagsdatoBackup' :: (MonadIO m) => FilePath -> DagsdatoBackup -> m ()
writeDagsdatoBackup' = writeJSONFile


--TODO could do some notification on save..
write :: (MonadIO m, MonadThrow m) => MVar FilePath -> DagsdatoBackup -> m ()
write file dagsdatoBackup' = liftIO $ withMVar file $ \f -> writeDagsdatoBackup' f dagsdatoBackup'


--TODO could handle error on write.
writeDagsdatoBackup :: (MonadIO m) => MVar FilePath -> DagsdatoBackup -> m ThreadId
writeDagsdatoBackup file dagsdatoBackup' = liftIO $ forkFinally (write file dagsdatoBackup') $ \ _ -> return ()


read :: (MonadIO m, MonadThrow m) => MVar FilePath -> Handler Model -> m (Either String DagsdatoBackup)
read file handle = liftIO $ withMVar file $ \f -> do
        --_ <- liftIO $ handle (Model Loading)
        getDagsdatoBackup' f


getDagsdatoBackup :: (MonadIO m, MonadThrow m) => MVar FilePath -> Handler Model -> m ()
getDagsdatoBackup file handle = liftIO $ (read file handle) >>= \case
            Left e' -> handle $ Model (Failure e')
            Right s -> handle $ Model (Data s)
