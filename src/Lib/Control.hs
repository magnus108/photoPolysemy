module Lib.Control
    ( controlXMP
    , Error(..)
    , parseRating
    , substring
    , Results(..)
    ) where

import qualified Control.Lens as Lens
import qualified Lib.Main as Main
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (unpack)
import qualified Lib.Doneshooting as Doneshooting
import qualified Lib.Rating as Rating
import qualified Lib.Grade as Grade
import qualified Lib.Camera as Camera
import qualified Lib.Location as Location
import Control.Exception
import System.Directory
import System.FilePath


between :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
between prefix' postfix' xs = rating
    where
        (_, after) = BS.breakSubstring prefix' xs
        (rating, _) = BS.breakSubstring postfix' after

substring :: String -> String -> Bool
substring _ [] = False
substring xs (y:ys)
    | prefix xs (y:ys) = True
    | substring xs ys = True
    | otherwise = False


prefix :: String -> String -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys


parseRating :: FilePath -> IO Rating.Rating
parseRating filepath = do
    content <- BS.readFile filepath
    seq content (return ())
    let prefix' = "<xmp:Rating>"
    let postfix' = "</xmp:Rating>"
    let rating = between prefix' postfix' content
    return (Rating.fromString (unpack rating))


data Error
    = Exactly1With5
    | Atleast5With1
    | CouldNotReadDoneshootingDir
    deriving (Show,Eq)


data Results = Results [Error]


controlXMP :: Main.Item -> IO Results
controlXMP item' = do
    let location = takeBaseName $ Location.unLocationFile $ Lens.view Main.location item'
    let doneshootingDir = Doneshooting.unDoneshooting $ Lens.view Main.doneshooting item'
    let grade = Grade.showGrade (Lens.view Main.grades item')
    let camera = Lens.view Main.camera item'
    let extension = snd $ Camera.toExtension camera
    let name = takeBaseName location
    let path = doneshootingDir </> name </> extension </> grade
    dir <- try $ listDirectory path :: IO (Either SomeException [FilePath])
    case dir of
        Left _ -> return (Results [CouldNotReadDoneshootingDir])
        Right _ -> do
            return (Results [])

            {-
                    let filtered = groupOn (\f -> (splitOn "."  f) !! 1) $ filter (\f -> isExtensionOf ext f) (sort files)
                    
                    let studentAndCrs = fmap (\xx -> ((splitOn "." (xx !! 0)) !! 1 , xx)) filtered

                    studentAndCrs' <- mapM (\xx -> do
                                res <- filterM (\f -> doesFileExist (path </> f -<.> "xmp")) (snd xx)
                                return (fst xx, res)
                                ) $ studentAndCrs

                    gg <- mapM (\xxx -> do
                            let xxxx = fst xxx
                            only1with5' <- mapM (only1With5_) $ (fmap (\xxxx -> (path </> xxxx))) (snd xxx)
                            let sum = 1 == (foldl (\ss acc -> ss + acc) 0 (only1with5'))
                            atleast5With1' <- mapM (atleast5With1_) $ (fmap (\xxxx -> (path </> xxxx))) (snd xxx)
                            let sum2 = 5 <= (foldl (\ss acc -> ss + acc) 0 (atleast5With1')) 
                            return (xxxx, sum, sum2)
                        ) studentAndCrs'

         
                    let yy = filter (\(xxxx, sum, sum2) -> not sum || not sum2 ) gg

                    let abc = fmap (\(xxxx, sum, sum2) ->  if ((not sum) &&  (not sum2)) then
                                    Errors xxxx [atleast5With1, exactly1With5]
                                else if (not sum) then
                                    Errors xxxx [atleast5With1]
                                else 
                                    Errors xxxx [exactly1With5] 
                                ) yy
                    return abc
-}
