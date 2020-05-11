{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Translation
    ( Translation(..)
    , starting
    , loading
    , next
    , prev
    , filePicker
    , photographersError
    , getTranslation'
    , read
    , writeTranslation'
    ) where

import Control.Lens
import Control.Concurrent

data Translation = Translation { _loading :: String
                               , _starting :: String
                               , _filePicker :: String
                               , _photographersError :: String
                               , _next :: String
                               , _prev :: String
                               }
    deriving (Show, Ord, Eq, Generic, ToJSON, FromJSON)

makeLenses ''Translation

getTranslation' :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String Translation)
getTranslation' = readJSONFile'

writeTranslation' :: (MonadIO m) => FilePath -> Translation -> m ()
writeTranslation' = writeJSONFile

read :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Translation)
read file = liftIO $ withMVar file $ \f -> do
    getTranslation' f


{-
newtype Model = Model { unModel :: Data String Photographers }


initalState :: Model
initalState = Model NotAsked


--TODO could do some notification on save..
write :: (MonadIO m, MonadThrow m) => MVar FilePath -> Photographers -> m ()
write file photographers = liftIO $ withMVar file $ \f -> writePhotographers' f photographers

--TODO could handle error on write.
writePhotographers :: (MonadIO m) => MVar FilePath -> Photographers -> m ThreadId
writePhotographers file photographers = liftIO $ forkFinally (write file photographers ) $ \ _ -> return ()


read :: (MonadIO m, MonadThrow m) => MVar FilePath -> Handler Model -> m (Either String Photographers)
read file handle = liftIO $ withMVar file $ \f -> do
        _ <- liftIO $ handle (Model Loading)
        getPhotographers' f


getPhotographers :: (MonadIO m, MonadThrow m) => MVar FilePath -> Handler Model -> m ThreadId
getPhotographers file handle = liftIO $ forkFinally (read file handle) $ \case
    Left e -> handle $ Model (Failure (show e))
    Right x -> case x of
            Left e' -> handle $ Model (Failure e')
            Right s -> handle $ Model (Data s)
            -}
