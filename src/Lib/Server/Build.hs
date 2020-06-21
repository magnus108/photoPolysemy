{-# LANGUAGE ScopedTypeVariables #-}
module Lib.Server.Build
    ( entry
    , getDate
    , mkDagsdatoBackupPath
    , mkDagsdatoPath
    , mkDoneshootingPath
    , mkDoneshootingPathJpg
    ) where

import Control.Concurrent (threadDelay, killThread, forkIO)
import qualified Control.Concurrent.Chan as Chan

import Data.Char

import Control.Exception

import Utils.Mealy

import Numeric.Extra
import System.Time.Extra

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
import qualified Development.Shake.FilePath as FP

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


myProgressProgram :: Int -> Chan.Chan Build.Build -> Photographee.Photographee -> IO Progress -> IO ()
myProgressProgram sample c photographee progress = do
    time <- offsetTime
    catchJust (\x -> if x == ThreadKilled then Just () else Nothing)
        (loop time $ message echoMealy)
        (const $ do t <- time
                    p <- progress
                    let todo = countBuilt p
                    Chan.writeChan c (Build.DoneBuild photographee (show (div todo 8)))
        )
    where
        loop :: IO Double -> Mealy (Double, Progress) (Double, Int) -> IO ()
        loop time mealy = do
            threadDelay sample
            t <- time
            p <- progress
            ((secs,todo), mealy) <- pure $ runMealy mealy (t, p)
            let f = isFailure p
            case f of
                Nothing -> do
                    Chan.writeChan c (Build.Building photographee (show (div todo 8)))
                Just _ ->
                    Chan.writeChan c (Build.NoBuild)
            loop time mealy


receiveMessages mfile msgs = do
    messages <- Chan.getChanContents msgs
    forM_ messages $ \msg -> do
        traceShowM msg
        Build.writeBuild mfile msg

opts :: Chan.Chan Build.Build -> Photographee.Photographee -> ShakeOptions
opts  c photographee = shakeOptions
                    { shakeFiles = shakeDir
                    , shakeProgress = progress -- should change
                    , shakeThreads = 5
                    , shakeColor = True
                    }
    where
        progress p = do
            myProgressProgram 50000 c photographee p


mkDoneshootingPath :: Int -> FilePath -> Main.Item -> FilePath
mkDoneshootingPath index' file item =
    Doneshooting.unDoneshooting doneshooting </> location </> extension </> grade </> sessionId ++ "." ++ tea ++ "." ++ shootingId ++ "." ++ photographerId ++ "." ++ no ++ (toLower <$> (FP.takeExtension file))
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
    Doneshooting.unDoneshooting doneshooting </> location </> extension </> "_webshop" </> sessionId ++ "." ++ tea ++ "." ++ shootingId ++ "." ++ photographerId ++ "." ++ no ++ (toLower <$> (FP.takeExtension file))
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
mkDagsdatoPath file date item = dagsdato </> date ++ " - " ++ location </> grade </> (name ++ " - " ++ tea) </> file -<.> (toLower <$> (FP.takeExtension file))
        where
            dagsdato = Dagsdato.unDagsdato $ Lens.view Main.dagsdato item
            location = takeBaseName $ Location.unLocationFile $ Lens.view Main.location item
            grade = Grade.showGrade (Lens.view Main.grades item)
            photographees = Lens.view Main.photographees item
            photographee = extract (Photographee.unPhotographees photographees)
            tea = Photographee.toTea' photographee
            name = Photographee.toName' photographee


mkDagsdatoBackupPath :: FilePath -> String -> Main.Item -> FilePath
mkDagsdatoBackupPath file date item = dagsdatoBackup </> date ++ " - " ++ location </> grade </> (name ++ " - " ++ tea) </> file -<.> (toLower <$> (FP.takeExtension file))
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

    messages <- Chan.newChan
    messageReceiver <- liftIO $ forkIO $ receiveMessages mBuildFile messages
    shaken <- try $ myShake (opts messages photographee) date item :: IO (Either SomeException ())
    killThread messageReceiver
    case shaken of
        Left _ -> Build.writeBuild mBuildFile (Build.NoBuild)
        Right _ -> Build.writeBuild mBuildFile (Build.DoneBuild photographee (""))


myShake :: ShakeOptions -> String -> Main.Item -> IO ()
myShake opts' time item = do
    let dump = Lens.view Main.dump item
    let root = Dump.unDump dump
    let dumpDir = Lens.view Main.dumpDir item
    let sortDir = sort (Dump.unDumpDir dumpDir)
    let tmp = Data.List.Index.imap (\index' cr -> do 
            let index'' = index' + 1
            let jpg = cr -<.> "jpg"

            let doneshootingCr = mkDoneshootingPath index'' cr item
            let doneshootingJpg = mkDoneshootingPathJpg index'' jpg item

            let dagsdatoCr = mkDagsdatoPath cr time item
            let dagsdatoJpg = mkDagsdatoPath jpg time item

            let dagsdatoBackupCr = mkDagsdatoBackupPath cr time item
            let dagsdatoBackupJpg = mkDagsdatoBackupPath jpg time item
            ((cr, (doneshootingCr,dagsdatoCr, dagsdatoBackupCr)), (jpg, (doneshootingJpg, dagsdatoJpg, dagsdatoBackupJpg)))
            ) sortDir
    case tmp of
        [] -> error "empty"
        xs -> do
            shake opts' $ do
                forM_ xs $ \ ((cr, (doneshootingCr,dagsdatoCr, dagsdatoBackupCr)),(jpg, (doneshootingJpg, dagsdatoJpg, dagsdatoBackupJpg))) -> do

                    want [doneshootingCr, doneshootingJpg, dagsdatoCr, dagsdatoJpg , dagsdatoBackupCr, dagsdatoBackupJpg]

                    doneshootingCr %> copyFile' (root </> cr)

                    doneshootingJpg %> copyFile' (root </> jpg)

                    dagsdatoCr %> copyFile' (root </> cr)

                    dagsdatoJpg %> copyFile' (root </> jpg)

                    dagsdatoBackupCr %> copyFile' (root </> cr)

                    dagsdatoBackupJpg %> copyFile' (root </> jpg)

                    action $ removeFilesAfter root ["//*.CR3", "//*.JPG", "//*.cr3", "//*.jpg","//*.CR2","//*.cr2"]
