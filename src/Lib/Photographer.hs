{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Photographer
    ( Photographer(..)
    , Photographers(..)
    , getPhotographers
    , getPhotographers'
    , writePhotographers
    , initalState
    , tid
    , Model(..)
    ) where

-- TODO get this import out
import Graphics.UI.Threepenny.Core

import Control.Concurrent
import Utils.ListZipper
import Control.Lens

import Lib.Data

type Name = String
type Tid = String

data Photographer = Photographer
    { _name :: Name
    , _tid :: Tid
    } deriving (Show, Ord, Eq, Generic, ToJSON, FromJSON)


makeLenses ''Photographer


newtype Photographers = Photographers { unPhotographers :: ListZipper Photographer }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


getPhotographers' :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String Photographers)
getPhotographers' = readJSONFile'

writePhotographers' :: (MonadIO m) => FilePath -> Photographers -> m ()
writePhotographers' = writeJSONFile



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
read file _ = liftIO $ withMVar file $ \f -> do
--        _ <- liftIO $ handle (Model Loading)
        getPhotographers' f


getPhotographers :: (MonadIO m, MonadThrow m) => MVar FilePath -> Handler Model -> m ()
getPhotographers file handle = liftIO $ (read file handle) >>= \case
            Left e' -> handle $ Model (Failure e')
            Right s -> handle $ Model (Data s)
