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
    , Data(..)
    ) where

import Graphics.UI.Threepenny.Core
import Control.Concurrent
import Utils.ListZipper

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











data Data e s
    = NotAsked
    | Loading
    | Failure e
    | Data s


newtype Model = Model { unModel :: Data String Photographers }


initalState :: Model
initalState = Model NotAsked


splitData :: Event (Data e s) -> (Event (), Event (), Event e, Event s)
splitData e = 
    (filterJust $ fromNotAsked <$> e
    , filterJust $ fromLoading <$> e
    , filterJust $ fromFailure <$> e
    , filterJust $ fromData <$> e
    )
    where
        fromLoading  Loading = Just ()
        fromLoading  _ = Nothing
        fromData (Data s) = Just s
        fromData _ = Nothing
        fromFailure (Failure e') = Just e'
        fromFailure _ = Nothing
        fromNotAsked NotAsked = Just ()
        fromNotAsked _ = Nothing


forker :: (MonadIO m, MonadThrow m) => MVar FilePath -> Handler (Data String Photographers) -> m (Either String Photographers)
forker file handle = do
    liftIO $ withMVar file $ \f -> do
        _ <- liftIO $ handle Loading
        _ <- threadDelay 5000000
        readJSONFile' f


getPhotographers :: (MonadIO m, MonadThrow m) => MVar FilePath -> Handler (Data String Photographers) -> m ThreadId
getPhotographers file handle = do
    liftIO $ forkFinally (forker file handle) $ \res -> do
        case res of
            Left e -> handle $ Failure (show e)
            Right x -> case x of
                    Left e' -> handle $ Failure e'
                    Right s -> handle $ Data s


