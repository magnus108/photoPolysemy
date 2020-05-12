{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Doneshooting
    ( Doneshooting(..)
    , Model(..)
    , getDoneshooting'
    , getDoneshooting
    , writeDoneshooting
    , initialState
    ) where


import Control.Concurrent
import Graphics.UI.Threepenny.Core

import Lib.Data

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


read :: (MonadIO m, MonadThrow m) => MVar FilePath -> Handler Model -> m (Either String Doneshooting)
read file handle = liftIO $ withMVar file $ \f -> do
        _ <- liftIO $ handle (Model Loading)
        getDoneshooting' f


getDoneshooting :: (MonadIO m, MonadThrow m) => MVar FilePath -> Handler Model -> m ThreadId
getDoneshooting file handle = liftIO $ forkFinally (read file handle) $ \case
    Left e -> handle $ Model (Failure (show e))
    Right x -> case x of
            Left e' -> handle $ Model (Failure e')
            Right s -> handle $ Model (Data s)
