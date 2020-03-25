{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

module Lib.Grade
    ( Grades(..)
    , Grade(..)
    , extractGrade
    , getGrades
    , showGrade
    , writeGrades
    , parseGrades
    ) where

import Utils.Comonad
import Utils.ListZipper
import Lib.Location


import Data.List (nub)
import Data.Csv
import qualified Data.Vector as Vector
import qualified Data.ByteString.Lazy as BL

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
parseGrades :: LocationFile -> IO (Either String Grades)
parseGrades locationFile = do
    data' <-  BL.readFile (unLocationFile locationFile)

    let locationData = decodeWith myOptionsDecode NoHeader $ data' :: Either String (Vector.Vector Photographee)

    case locationData of
            Left _ -> return (Left "fejl")
            Right locData -> do
                let grades = nub $ Vector.toList $ fmap _grade locData
                case grades of
                    [] -> return (Left "fejl")
                    x:xs -> return $ Right $ Grades $ fmap Grade $ ListZipper [] x xs



getGrades :: (MonadIO m, MonadThrow m) => FilePath -> m Grades
getGrades = readJSONFile


writeGrades :: (MonadIO m) => FilePath -> Grades -> m ()
writeGrades = writeJSONFile
