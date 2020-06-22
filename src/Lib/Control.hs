module Lib.Control
    ( controlXMP
    , Error(..)
    , translationError
    , parseRating
    , _unResults
    , Results(..)
    , between
    ) where
import qualified Lib.Translation as Translation --todo should not be here

import Data.Strings

import qualified Utils.ListZipper as ListZipper
import Data.List.Extra hiding (elem)
import qualified Control.Lens as Lens
import qualified Lib.ControlModel as ControlModel
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (unpack)
import qualified Lib.Doneshooting as Doneshooting
import qualified Lib.Rating as Rating
import qualified Lib.Photographee as Photographee
import qualified Lib.Grade as Grade
import qualified Lib.Camera as Camera
import qualified Lib.Location as Location
import Control.Exception
import System.Directory
import System.FilePath


between :: String -> String -> String -> String
between prefix' postfix' xs = rating
    where
        (_, after) = strSplit prefix' xs
        (rating, _) = strBreak postfix' after

parseRating :: FilePath -> IO Rating.Rating
parseRating filepath = do
    content <- BS.readFile filepath
    seq content (return ())
    let prefix' = "<xmp:Rating>"
    let postfix' = "</xmp:Rating>"
    let rating = between prefix' postfix' (unpack content)
    return (Rating.fromString rating)


data Error
    = Exactly1With5
    | Atleast5With1
    | CouldNotReadDoneshootingDir
    deriving (Show,Eq)


newtype Results = Results { _unResults :: [(Photographee.Photographee, Error)] }

--
--TODO this is rediculose
translationError :: Error -> Translation.Translation -> String
translationError err = Lens.view translator
    where translator = case err of
            Exactly1With5 -> Translation.exactly1With5
            Atleast5With1 -> Translation.atleast5With1


controlXMP :: ControlModel.Item -> IO Results
controlXMP item' = do
    let files = Doneshooting.unDoneshootingDir $ Lens.view ControlModel.doneshootingDir item'
    let photographees' = ListZipper.toList $ Photographee.unPhotographees $ Lens.view ControlModel.photographees item'
    let xmps = fst $ snd files
    let root = snd $ snd files
    let crs = fst files


    let pairPhotographeeAndCr = map (\photographee' ->  (photographee', filter (\file' -> isInfixOf (Photographee.toIdent' photographee') file') crs)) photographees'
    let pairPhotographeeAndCrAndXmp = 
            (\i -> do
                (i, catMaybes $ map (\cr -> find (\xmp -> cr -<.> "xmp" == xmp) xmps) (snd i))
            ) <$> pairPhotographeeAndCr 

    parsedAndRdy <- mapM (\x -> do 
            rate <- mapM (\y -> parseRating (root </> y)) (snd x)
            return (fst x,rate)) pairPhotographeeAndCrAndXmp

    let gg = fmap (\i -> if (Rating.five `elem` (snd i)) then Nothing else Just (i, Exactly1With5)) parsedAndRdy
    let gg2 = fmap (\i -> if length (filter (\i' -> Rating.toInt i' > 1) (snd i)) > 5 then Nothing else Just (i, Atleast5With1)) parsedAndRdy

    let allErro = fmap (\i -> (fst (fst( fst i)), snd i)) $ catMaybes $ gg++gg2
    
    return (Results allErro)

            {-
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
