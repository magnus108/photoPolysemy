{-# LANGUAGE ScopedTypeVariables #-}
module Lib.Server.Build
    ( entry
    , getDate
    , myShake'
    , mkDagsdatoBackupPath
    , mkDagsdatoPath
    , mkDoneshootingPath
    , mkDoneshootingPathJpg
    ) where

import Data.Char

import Control.Exception
import Control.Concurrent

import Utils.Mealy

import Numeric.Extra
import System.Time.Extra

import System.FilePath
import System.Directory (listDirectory)
import Control.Exception
import Control.Concurrent (withMVar)

import qualified Data.String as String
import qualified Data.List.Index
import Data.Strings

import Utils.Comonad

import qualified Lib.Main as Main
import qualified Control.Lens as Lens

import qualified Lib.Build as Build
import qualified Lib.Shooting as Shooting
import qualified Lib.Photographer as Photographer
import qualified Lib.Photographee as Photographee
import qualified Lib.Session as Session
import qualified Lib.Location as Location
import qualified Lib.Grade as Grade
import qualified Lib.Dump as Dump
import qualified Lib.Doneshooting as Doneshooting
import qualified Lib.Dagsdato as Dagsdato
import qualified Lib.DagsdatoBackup as DagsdatoBackup
import qualified Lib.Camera as Camera

import Development.Shake
import Development.Shake.FilePath

import Data.Time.Format
import Data.Time.Clock










shakeDir :: FilePath
shakeDir = "._build"

getDate :: UTCTime -> String
getDate = formatTime defaultTimeLocale "%Y - %m%d"


message :: Mealy (Double, Progress) (Double, Progress) -> Mealy (Double, Progress) (Double, Int)
message input = liftA2 (,) done todo
    where
        progress = snd <$> input
        secs = fst <$> input
        done = timeBuilt <$> progress
        todo = countBuilt <$> progress

liftA2' :: Applicative m => m a -> m b -> (a -> b -> c) -> m c
liftA2' a b f = liftA2 f a b


myProgressProgram :: Int -> MVar FilePath -> Photographee.Photographee -> IO Progress -> IO ()
myProgressProgram sample mBuildFile photographee progress = do
    time <- offsetTime
    (loop time $ message echoMealy)
    where
        loop :: IO Double -> Mealy (Double, Progress) (Double, Int) -> IO ()
        loop time mealy = do
            threadDelay sample
            t <- time
            p <- progress
            ((secs,perc), mealy) <- pure $ runMealy mealy (t, p)
            Build.write mBuildFile (Build.Building photographee (show (div perc 8)))


opts :: MVar FilePath -> Photographee.Photographee -> ShakeOptions
opts mBuildFile photographee = shakeOptions
                    { shakeFiles = shakeDir
                    , shakeProgress = progress -- should change
                    , shakeThreads = 5
                    , shakeColor = True
                    }
    where
        progress p = do
            myProgressProgram 1000000 mBuildFile photographee p


mkDoneshootingPath :: Int -> FilePath -> Main.Item -> FilePath
mkDoneshootingPath index' file item =
    Doneshooting.unDoneshooting doneshooting </> location </> extension </> grade </> sessionId ++ "." ++ tea ++ "." ++ shootingId ++ "." ++ photographerId ++ "." ++ no ++ (toLower <$> (takeExtension file))
        where
            location = takeBaseName $ Location.unLocationFile $ Lens.view Main.location item
            session = Lens.view Main.session item
            sessionId = show $ Session.toInteger session

            camera = Lens.view Main.camera item
            extension = snd $ Camera.toExtension camera
            photographer = Lens.view Main.photographer item
            photographerId = Lens.view Photographer.tid photographer
            doneshooting = Lens.view Main.doneshooting item
            shooting = Lens.view Main.shooting item
            shootingId = if session == Session.KindergartenGroup then "3" else show $ Shooting.toInteger shooting
            grade = Grade.showGrade (Lens.view Main.grades item)
            photographees = Lens.view Main.photographees item
            photographee = extract (Photographee.unPhotographees photographees)
            tea = Photographee.toTea' photographee
            pad x = strPadLeft '0' 3 (show x)
            no = pad index'


mkDoneshootingPathJpg :: Int -> FilePath -> Main.Item -> FilePath
mkDoneshootingPathJpg index' file item =
    Doneshooting.unDoneshooting doneshooting </> location </> extension </> "_webshop" </> sessionId ++ "." ++ tea ++ "." ++ shootingId ++ "." ++ photographerId ++ "." ++ no ++ (toLower <$> (takeExtension file))
        where
            location = takeBaseName $ Location.unLocationFile $ Lens.view Main.location item
            session = Lens.view Main.session item
            sessionId = show $ Session.toInteger session

            camera = Lens.view Main.camera item
            extension = snd $ Camera.toExtension camera
            photographer = Lens.view Main.photographer item
            photographerId = Lens.view Photographer.tid photographer
            doneshooting = Lens.view Main.doneshooting item
            shooting = Lens.view Main.shooting item
            shootingId = if session == Session.KindergartenGroup then "3" else show $ Shooting.toInteger shooting
            photographees = Lens.view Main.photographees item
            photographee = extract (Photographee.unPhotographees photographees)
            tea = Photographee.toTea' photographee
            pad x = strPadLeft '0' 3 (show x)
            no = pad index'


mkDagsdatoPath :: FilePath -> String -> Main.Item -> FilePath
mkDagsdatoPath file date item = dagsdato </> date ++ " - " ++ location </> grade </> (name ++ " - " ++ tea) </> file -<.> (toLower <$> (takeExtension file))
        where
            dagsdato = Dagsdato.unDagsdato $ Lens.view Main.dagsdato item
            location = takeBaseName $ Location.unLocationFile $ Lens.view Main.location item
            grade = Grade.showGrade (Lens.view Main.grades item)
            photographees = Lens.view Main.photographees item
            photographee = extract (Photographee.unPhotographees photographees)
            tea = Photographee.toTea' photographee
            name = Photographee.toName' photographee


mkDagsdatoBackupPath :: FilePath -> String -> Main.Item -> FilePath
mkDagsdatoBackupPath file date item = dagsdatoBackup </> date ++ " - " ++ location </> grade </> (name ++ " - " ++ tea) </> file -<.> (toLower <$> (takeExtension file))
        where
            dagsdatoBackup = DagsdatoBackup.unDagsdatoBackup $ Lens.view Main.dagsdatoBackup item
            location = takeBaseName $ Location.unLocationFile $ Lens.view Main.location item
            grade = Grade.showGrade (Lens.view Main.grades item)
            photographees = Lens.view Main.photographees item
            photographee = extract (Photographee.unPhotographees photographees)
            name   = Photographee.toName' photographee
            tea   = Photographee.toTea' photographee


entry :: MVar FilePath -> Main.Item -> IO ()
entry mBuildFile item = do
    time <- getCurrentTime
    let date = getDate time

    let photographees = Lens.view Main.photographees item
    let photographee = extract (Photographee.unPhotographees photographees)
    shaken <- try $ myShake mBuildFile (opts mBuildFile photographee) date item :: IO (Either SomeException ())
    case shaken of
        Left _ -> Build.write mBuildFile (Build.NoBuild)
        Right _ -> Build.write mBuildFile (Build.DoneBuild photographee "")


myShake :: MVar FilePath -> ShakeOptions -> String -> Main.Item -> IO ()
myShake mBuildFile opts' time item = do
    let dumpDir = Lens.view Main.dumpDir item
    checkDump <- Dump.checkDumpFiles (Lens.view Main.dump item) (Lens.view Main.camera item)
    case  checkDump of
      Left _ ->
            void $ Build.write mBuildFile (Build.NoBuild)
      Right _ -> 
            if length (Dump.unDumpDir dumpDir) == 0 then
                void $ Build.write mBuildFile (Build.NoBuild)
            else
                myShake' opts' time item

myShake' :: ShakeOptions -> String -> Main.Item -> IO ()
myShake' opts' time item = shake opts' $ do

        let dump = Lens.view Main.dump item
        let dumpDir = Lens.view Main.dumpDir item

        Data.List.Index.ifor_ (sort (Dump.unDumpDir dumpDir)) $ \ index' cr -> do
            let root = Dump.unDump dump
            let index'' = index' + 1
            let jpg = cr -<.> "jpg"

            let doneshootingCr = mkDoneshootingPath index'' cr item
            let doneshootingJpg = mkDoneshootingPathJpg index'' jpg item

            let dagsdatoCr = mkDagsdatoPath cr time item
            let dagsdatoJpg = mkDagsdatoPath jpg time item

            let dagsdatoBackupCr = mkDagsdatoBackupPath cr time item
            let dagsdatoBackupJpg = mkDagsdatoBackupPath jpg time item

            want [doneshootingCr, doneshootingJpg, dagsdatoCr, dagsdatoJpg , dagsdatoBackupCr, dagsdatoBackupJpg]

            doneshootingCr %> copyFile' (root </> cr)

            doneshootingJpg %> copyFile' (root </> jpg)

            dagsdatoCr %> copyFile' (root </> cr)

            dagsdatoJpg %> copyFile' (root </> jpg)

            dagsdatoBackupCr %> copyFile' (root </> cr)

            dagsdatoBackupJpg %> copyFile' (root </> jpg)

            action $ removeFilesAfter root ["//*.CR3", "//*.JPG", "//*.cr3", "//*.jpg","//*.CR2","//*.cr2"]
