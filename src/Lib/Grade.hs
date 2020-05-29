{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

module Lib.Grade
    ( Grades(..)
    , Grade(..)
    , Model(..)
    , grades
    , extractGrade
    , getGrades
    , getGrades'
    , initialState
    , showGrade
    , writeGrades'
    , writeGrades
    ) where

import Control.Concurrent

import Utils.Comonad
import Utils.ListZipper

import Lib.Data

import Control.Lens
import Graphics.UI.Threepenny.Core

newtype Grade = Grade { unGrade :: String }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


newtype Grades = Grades { unGrades :: ListZipper Grade }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


extractGrade :: Grades -> Grade
extractGrade = extract . unGrades

showGrade :: Grades -> String
showGrade = unGrade . extract . unGrades


getGrades' :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String Grades)
getGrades' = readJSONFile'


writeGrades' :: (MonadIO m) => FilePath -> Grades -> m ()
writeGrades' = writeJSONFile



--TODO could do some notification on save..
write :: (MonadIO m, MonadThrow m) => MVar FilePath -> Grades -> m ()
write file grades = liftIO $ withMVar file $ \f -> writeGrades' f grades


--TODO could handle error on write.
writeGrades :: (MonadIO m) => MVar FilePath -> Grades -> m ThreadId
writeGrades file grades = liftIO $ forkFinally (write file grades) $ \ _ -> return ()


data Model = Model { _grades :: Data String Grades } deriving Show

makeLenses ''Model


initialState :: Model
initialState = Model NotAsked


forker :: (MonadIO m, MonadThrow m) => MVar FilePath -> Handler Model -> m (Either String Grades)
forker file _ = do
    liftIO $ withMVar file $ \f -> do
        ---_ <- liftIO $ handle $ Model Loading
        getGrades' f


getGrades :: (MonadIO m, MonadThrow m) => MVar FilePath -> Handler Model -> m ()
getGrades file handle = do
    liftIO $ (forker file handle) >>= \case
                    Left e' -> handle $ Model $ Failure e'
                    Right s -> handle $ Model $ Data s
