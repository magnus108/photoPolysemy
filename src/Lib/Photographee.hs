{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Photographee
    ( Photographee(..)
    , Photographees(..)
    , name
    , unModel
    , fromGrade
    , initialState
    , parseGrades
    , Model(..)
    , getPhotographees
    ) where

import Control.Concurrent
import Data.Csv
import Data.List (nub)

import Graphics.UI.Threepenny.Core

import Lib.Data
import Control.Lens

import Utils.Comonad
import qualified Utils.ListZipper as ListZipper
import Control.Lens

import qualified Data.Vector as Vector
import qualified Data.ByteString.Lazy as BL

import qualified Lib.Location as Location
import qualified Lib.Grade as Grade

data Photographee = Photographee
    { _tea :: String
    , _grade :: String --should be of type grade no?
    , _name :: String
    , _ident :: String
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)


makeLenses ''Photographee


newtype Photographees = Photographees { unPhotographees :: ListZipper.ListZipper Photographee }
    deriving (Eq, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


instance FromRecord Photographee
instance ToRecord Photographee

photographee :: String -> String -> String -> String -> Photographee
photographee = Photographee


myOptionsDecode :: DecodeOptions
myOptionsDecode = defaultDecodeOptions { decDelimiter = fromIntegral (ord ';') }

myOptionsEncode :: EncodeOptions
myOptionsEncode = defaultEncodeOptions { encDelimiter = fromIntegral (ord ';') }


fromGrade :: Location.LocationFile -> Grade.Grades -> IO (Either String Photographees)
fromGrade locationFile grades = do
    data' <- BL.readFile (Location.unLocationFile locationFile)

    let locationData = decodeWith myOptionsDecode NoHeader $ data' :: Either String (Vector.Vector Photographee)

    case locationData of
            Left _ -> return (Left "fejl")
            Right locData -> do
                let photographees = Vector.filter (((Grade.unGrade (extract (Grade.unGrades grades))) ==) . _grade) locData
                let zipper = ListZipper.fromList $ Vector.toList photographees
                case zipper of
                    Nothing -> return (Left "fejl")
                    Just zs -> return (Right (Photographees zs))


parseGrades :: Location.LocationFile -> IO (Either String Grade.Grades)
parseGrades locationFile = do
    data' <-  BL.readFile (Location.unLocationFile locationFile)

    let locationData = decodeWith myOptionsDecode NoHeader $ data' :: Either String (Vector.Vector Photographee)

    case locationData of
            Left _ -> return (Left "fejl")
            Right locData -> do
                let grades = nub $ Vector.toList $ fmap _grade locData
                case grades of
                    [] -> return (Left "fejl")
                    x:xs -> return $ Right $ Grade.Grades $ fmap Grade.Grade $ ListZipper.ListZipper [] x xs


--------------------------------------------------------------------------------

data Model = Model { _unModel :: Data String Photographees } deriving Show

makeLenses ''Model


initialState :: Model
initialState = Model NotAsked


forker :: (MonadIO m, MonadThrow m) => MVar FilePath -> MVar FilePath -> Handler Model -> m (Either String Photographees)
forker mGradeFile mLocationConfigFile handle = do
    _ <- liftIO $ handle $ Model Loading
    liftIO $ withMVar mLocationConfigFile $ \f -> do
        locationFile <- Location.getLocationFile' f --TODO fix this shit
        case locationFile of
            Left x -> return $ Left x
            Right locafile -> do
                withMVar mGradeFile $ \gfile -> do
                        grades <- Grade.getGrades' gfile --TODO fix this shit
                        case grades of
                            Left x -> return $ Left x
                            Right grada -> do
                                fromGrade locafile grada


getPhotographees :: (MonadIO m, MonadThrow m) => MVar FilePath -> MVar FilePath -> Handler Model -> m ThreadId
getPhotographees mGradeFile mLocationConfigFile handle = do
    liftIO $ forkFinally (forker mGradeFile mLocationConfigFile handle) $ \res -> do
        case res of
            Left e -> handle $ Model $ Failure (show e)
            Right x -> case x of
                    Left e' -> handle $ Model $ Failure e'
                    Right s -> handle $ Model $ Data s

    {-
findPhotographee :: Location.Location -> Id.Id -> IO (Maybe Photographee)
findPhotographee location id = do
    Location.location (return Nothing) (\l -> do 
        b <- SD.doesFileExist l
        case b of
            True -> do
                locationData' <- BL.readFile l
                let locationData = decodeWith myOptionsDecode NoHeader $ locationData'
                --could use some case of here and error handling
                let studentData = case locationData of
                        Left _ -> throw ParseLocationFile
                        Right locData -> Id.id Nothing 
                                (\i -> List.find ((i ==) . _ident ) locData) id
                return studentData
            False -> return Nothing
        ) location


insert :: Location.Location -> Grade.Grade -> String -> String -> Maybe (IO ())
insert location grade id name = do
    Location.location (Nothing) (\l -> Just $ do 
        locationData' <- BL.readFile l
        let locationData = decodeWith myOptionsDecode NoHeader $ locationData' :: Either String (Vector.Vector Photographee)

        let low = 1000000 :: Int
        let high = 9999999 :: Int
        r <- getStdRandom (randomR (low, high))
        let studentData = case locationData of
                Left _ -> throw ParseLocationFile
                Right locData -> locData Vector.++ (Vector.fromList [photographee ("SYS_" List.++ id) grade name ("ny_" List.++ (show r))])

        let moreData = encodeWith myOptionsEncode $ Vector.toList studentData --can throw error

        seq (BL.length locationData') (BL.writeFile l moreData)
        ) location

            -}

