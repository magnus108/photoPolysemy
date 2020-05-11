{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Dagsdato
    ( Dagsdato(..)
    , Model(..)
    , getDagsdato'
    , getDagsdato
    , writeDagsdato
    , initialState
    ) where


import Control.Concurrent
import Graphics.UI.Threepenny.Core

import Lib.Data

import Control.Lens

newtype Dagsdato = Dagsdato { unDagsdato :: FilePath }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


newtype Model = Model { unModel :: Data String Dagsdato }

makeLenses ''Model

initialState :: Model
initialState = Model NotAsked

getDagsdato' :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String Dagsdato)
getDagsdato' = readJSONFile'


writeDagsdato' :: (MonadIO m) => FilePath -> Dagsdato -> m ()
writeDagsdato' = writeJSONFile


--TODO could do some notification on save..
write :: (MonadIO m, MonadThrow m) => MVar FilePath -> Dagsdato -> m ()
write file dagsdato' = liftIO $ withMVar file $ \f -> writeDagsdato' f dagsdato'


--TODO could handle error on write.
writeDagsdato :: (MonadIO m) => MVar FilePath -> Dagsdato -> m ThreadId
writeDagsdato file dagsdato' = liftIO $ forkFinally (write file dagsdato') $ \ _ -> return ()


read :: (MonadIO m, MonadThrow m) => MVar FilePath -> Handler Model -> m (Either String Dagsdato)
read file handle = liftIO $ withMVar file $ \f -> do
        _ <- liftIO $ handle (Model Loading)
        getDagsdato' f


getDagsdato :: (MonadIO m, MonadThrow m) => MVar FilePath -> Handler Model -> m ThreadId
getDagsdato file handle = liftIO $ forkFinally (read file handle) $ \case
    Left e -> handle $ Model (Failure (show e))
    Right x -> case x of
            Left e' -> handle $ Model (Failure e')
            Right s -> handle $ Model (Data s)
