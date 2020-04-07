{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

module Lib.Photographer
    ( Photographer(..)
    , Photographers(..)
    , getPhotographers
    , getPhotographers'
    , writePhotographers
    , initalState
    , splitData
    , Model(..)
    ) where

import Graphics.UI.Threepenny.Core
import Control.Concurrent
import Utils.ListZipper

import Lib.Data

type Name = String
type Tid = String

data Photographer = Photographer
    { _name :: Name
    , _tid :: Tid
    --TODO --ord?
    } deriving (Show, Ord, Eq, Generic, ToJSON, FromJSON)


newtype Photographers = Photographers { unPhotographers :: ListZipper Photographer }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


getPhotographers' :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String Photographers)
getPhotographers' = readJSONFile'

writePhotographers :: (MonadIO m) => FilePath -> Photographers -> m ()
writePhotographers = writeJSONFile








newtype Model = Model { unModel :: Data String Photographers }


initalState :: Model
initalState = Model NotAsked

forker :: (MonadIO m, MonadThrow m) => MVar FilePath -> Handler (Data String Photographers) -> m (Either String Photographers)
forker file handle = do
    liftIO $ withMVar file $ \f -> do
        _ <- liftIO $ handle Loading
        getPhotographers' f


getPhotographers :: (MonadIO m, MonadThrow m) => MVar FilePath -> Handler (Data String Photographers) -> m ThreadId
getPhotographers file handle = do
    liftIO $ forkFinally (forker file handle) $ \res -> do
        case res of
            Left e -> handle $ Failure (show e)
            Right x -> case x of
                    Left e' -> handle $ Failure e'
                    Right s -> handle $ Data s
