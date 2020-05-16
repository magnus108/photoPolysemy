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
    , initialState
    , showGrade
    , writeGrades'
    , writeGrades
    , parseGrades
    ) where

import Control.Concurrent

import Utils.Comonad
import Utils.ListZipper
import qualified Lib.Location as Location

import Lib.Data

import Control.Lens
import Data.List (nub)
import Data.Csv
import qualified Data.Vector as Vector
import qualified Data.ByteString.Lazy as BL
import Graphics.UI.Threepenny.Core

newtype Grade = Grade { unGrade :: String }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


newtype Grades = Grades { unGrades :: ListZipper Grade }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


--TODO move me
data Photographee = Photographee
    { _tea :: String
    , _grade :: String
    , _name :: String
    , _ident :: String
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromRecord Photographee
instance ToRecord Photographee

extractGrade :: Grades -> Grade
extractGrade = extract . unGrades

showGrade :: Grades -> String
showGrade = unGrade . extract . unGrades


--TODO move me
myOptionsDecode :: DecodeOptions
myOptionsDecode = defaultDecodeOptions { decDelimiter = fromIntegral (ord ';') }

--TODO move me
parseGrades :: Location.LocationFile -> IO (Either String Grades)
parseGrades locationFile = do
    data' <-  BL.readFile (Location.unLocationFile locationFile)

    let locationData = decodeWith myOptionsDecode NoHeader $ data' :: Either String (Vector.Vector Photographee)

    case locationData of
            Left _ -> return (Left "fejl")
            Right locData -> do
                let grades = nub $ Vector.toList $ fmap _grade locData
                case grades of
                    [] -> return (Left "fejl")
                    x:xs -> return $ Right $ Grades $ fmap Grade $ ListZipper [] x xs

-------------------------------------------------------------------------------

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
forker file handle = do
    liftIO $ withMVar file $ \f -> do
        _ <- liftIO $ handle $ Model Loading
        getGrades' f


getGrades :: (MonadIO m, MonadThrow m) => MVar FilePath -> Handler Model -> m ThreadId
getGrades file handle = do
    liftIO $ forkFinally (forker file handle) $ \res -> do
        case res of
            Left e -> handle $ Model $ Failure (show e)
            Right x -> case x of
                    Left e' -> handle $ Model $ Failure e'
                    Right s -> handle $ Model $ Data s
