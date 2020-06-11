{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Photographee
    ( Photographee(..)
    , Photographees(..)
    , setName
    , setIdent
    , empty
    , insert
    , name
    , toName
    , toIdent
    , photographee
    , ident
    , unModel
    , tea
    , fromGrade
    , initialState
    , parseGrades
    , writePhotographees
    , Model(..)
    , reloadPhotographees
    , getPhotographees
    , tryFindById
    ) where

import Control.Concurrent
import Data.Csv
import Data.List (nub)

import Prelude hiding (empty)

import Lib.Data
import Control.Lens

import Utils.Comonad
import qualified Utils.ListZipper as ListZipper

import qualified Data.Vector as Vector
import qualified Data.ByteString.Lazy as BL

import qualified Lib.Location as Location
import qualified Lib.Grade as Grade

data Photographee = Photographee
    { _tea :: String
    , _name :: String
    , _ident :: String
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)


data PhotographeeData = ParsePhotographeeData
    { teaData :: String
    , gradeData :: String
    , nameData :: String
    , identData :: String
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)


makeLenses ''Photographee


newtype Photographees = Photographees { unPhotographees :: ListZipper.ListZipper Photographee}
        deriving (Eq, Show)
        deriving (Generic)
        deriving (FromJSON, ToJSON)


tryFindById :: String -> Photographees -> Photographees
tryFindById s (Photographees x) = 
    let 
        found = ListZipper.findFirst (\y -> (_ident y) == s) x
    in 
        case found of
            Nothing -> Photographees x
            Just xs -> Photographees xs


setName :: String -> Photographees -> Photographees
setName name' (Photographees xs) = Photographees $ 
    ListZipper.mapFocus (\x -> photographee (_tea x) name' (_ident x)) xs

setIdent :: String -> Photographees -> Photographees
setIdent ident' (Photographees xs) = Photographees $
    ListZipper.mapFocus (\x -> photographee (_tea x) (_name x) ident') xs


toName :: Photographees -> String
toName (Photographees x) = _name ( extract x)

toIdent :: Photographees -> String
toIdent (Photographees x) =  _ident ( extract x)



instance FromRecord PhotographeeData
instance ToRecord PhotographeeData

photographee :: String -> String -> String -> Photographee
photographee = Photographee


empty :: Photographee
empty = photographee "" "" "" 


insert :: Photographee -> Photographees -> Photographees
insert x (Photographees xs) = Photographees $ ListZipper.insert xs x


myOptionsDecode :: DecodeOptions
myOptionsDecode = defaultDecodeOptions { decDelimiter = fromIntegral (ord ';') }

--myOptionsEncode :: EncodeOptions
--myOptionsEncode = defaultEncodeOptions { encDelimiter = fromIntegral (ord ';') }


fromGrade :: Location.LocationFile -> Grade.Grades -> IO (Either String Photographees)
fromGrade locationFile grades = do
    data' <- BL.readFile (Location.unLocationFile locationFile)

    let locationData = decodeWith myOptionsDecode NoHeader $ data' :: Either String (Vector.Vector PhotographeeData)

    case locationData of
            Left _ -> return (Left "fejl")
            Right locData -> do
                let photographees = Vector.filter (((view Grade.unGrade (extract (view Grade.unGrades grades))) ==) . gradeData) locData
                let zipper = ListZipper.fromList $ fmap (\x -> photographee (teaData x) (nameData x) (identData x)) $ Vector.toList photographees
                case zipper of
                    Nothing -> return (Left "fejl")
                    Just zs -> return (Right (Photographees zs))


parseGrades :: Location.LocationFile -> IO (Either String Grade.Grades)
parseGrades locationFile = do
    data' <-  BL.readFile (Location.unLocationFile locationFile)

    let locationData = decodeWith myOptionsDecode NoHeader $ data' :: Either String (Vector.Vector PhotographeeData)

    case locationData of
            Left _ -> return (Left "fejl")
            Right locData -> do
                let grades = nub $ Vector.toList $ fmap gradeData locData
                case grades of
                    [] -> return (Left "fejl")
                    x:xs -> return $ Right $ Grade.Grades $ fmap Grade.Grade $ ListZipper.ListZipper [] x xs


--------------------------------------------------------------------------------

data Model = Model { _unModel :: Data String Photographees } deriving Show

makeLenses ''Model


initialState :: Model
initialState = Model NotAsked


getPhotographees' :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String Photographees)
getPhotographees' = readJSONFile'


writePhotographees' :: (MonadIO m) => FilePath -> Photographees -> m ()
writePhotographees' = writeJSONFile


--TODO could do some notification on save..
write :: (MonadIO m, MonadThrow m) => MVar FilePath -> Photographees -> m ()
write file photographees = liftIO $ withMVar file $ \f -> writePhotographees' f photographees

--TODO could handle error on write.
writePhotographees :: (MonadIO m) => MVar FilePath -> Photographees -> m ()
writePhotographees file photographees = liftIO $ (write file photographees)


read :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Photographees)
read file = liftIO $ withMVar file $ \f -> do
--        _ <- liftIO $ handle (Model Loading)
        getPhotographees' f


getPhotographees :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Photographees)
getPhotographees file = liftIO $ read file


-------------------------------------------------------------------------------------------------------------------------
reloadForker :: (MonadIO m, MonadThrow m) => MVar FilePath -> MVar FilePath -> m (Either String Photographees)
reloadForker mGradeFile mLocationConfigFile = do
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


reloadPhotographees :: (MonadIO m, MonadThrow m) => MVar FilePath -> MVar FilePath -> MVar FilePath -> m ()
reloadPhotographees mGradeFile mLocationConfigFile mPhotographeesFile = do
    liftIO $ (reloadForker mGradeFile mLocationConfigFile ) >>= \case
                    Left _ -> return ()
                    Right y -> do
                        void $ writePhotographees mPhotographeesFile y





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

