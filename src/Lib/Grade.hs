{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

module Lib.Grade
    ( Grades(..)
    , Grade(..)
    , Model(..)
    , inputGrade
    , mkNewGrade
    , grades
    , unGrades
    , unGrade
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

newtype Grade = Grade { _unGrade :: String }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)

makeLenses ''Grade

newtype Grades = Grades { _unGrades :: ListZipper Grade }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)

makeLenses ''Grades


extractGrade :: Grades -> Grade
extractGrade = extract . (view unGrades)

showGrade :: Grades -> String
showGrade = view unGrade . extract . view unGrades

mkNewGrade :: Grades -> Grades
mkNewGrade grades' =
    grades' & unGrades %~ (\x -> insert x newGrade)
        where
            newGrade = Grade ""

inputGrade :: String -> Grades -> Grades
inputGrade name grades' =
    grades' & unGrades %~ mapFocus (const (Grade name))


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


forker :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Grades)
forker file = liftIO $ withMVar file $ \f -> do
        getGrades' f


getGrades :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Grades)
getGrades file = liftIO $ forker file
