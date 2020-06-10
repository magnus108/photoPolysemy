module Lib.Server.Build
    ( entry
    ) where

import qualified Data.List.Index
import Control.Concurrent
import Data.Strings
import Graphics.UI.Threepenny.Core

import Control.Lens
import Utils.Comonad

import qualified Lib.Main as Main
import qualified Control.Lens as Lens

import Lib.Data
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

opts :: MVar FilePath -> Photographee.Photographee -> ShakeOptions
opts mBuildFile photographee = shakeOptions
                    { shakeFiles = shakeDir
                    , shakeProgress = progress -- should change
                    , shakeThreads = 0
                    , shakeColor = True
                    }
    where
        progress p = do
            progressDisplay 0.05 (\s -> do
                case s of
                    "" -> Build.write mBuildFile (Build.NoBuild)
                    x -> case words (show x) of
                            "Finished":_ -> Build.write mBuildFile (Build.DoneBuild photographee x)
                            _ -> Build.write mBuildFile (Build.Building photographee x)
                ) p


mkDoneshootingPath :: Int -> FilePath -> Main.Item -> FilePath
mkDoneshootingPath index file item =
    Doneshooting.unDoneshooting doneshooting </> location </> extension </> grade </> sessionId ++ "." ++ tea ++ "." ++ shootingId ++ "." ++ photographerId ++ "." ++ no ++ (takeExtension file)
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
            shootingId = show $ Shooting.toInteger shooting
            grade = Grade.showGrade (Lens.view Main.grades item)
            photographees = Lens.view Main.photographees item
            photographee = extract (Photographee.unPhotographees photographees)
            tea = Lens.view Photographee.tea photographee
            pad x = strPadLeft '0' 3 (show x)
            no = pad index


mkDoneshootingPathJpg :: Int -> FilePath -> Main.Item -> FilePath
mkDoneshootingPathJpg index file item =
    Doneshooting.unDoneshooting doneshooting </> location </> extension </> "_webshop" </> sessionId ++ "." ++ tea ++ "." ++ shootingId ++ "." ++ photographerId ++ "." ++ no ++ (takeExtension file)
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
            shootingId = show $ Shooting.toInteger shooting
            grade = Grade.showGrade (Lens.view Main.grades item)
            photographees = Lens.view Main.photographees item
            photographee = extract (Photographee.unPhotographees photographees)
            tea = Lens.view Photographee.tea photographee
            pad x = strPadLeft '0' 3 (show x)
            no = pad index


mkDagsdatoPath :: FilePath -> String -> Main.Item -> FilePath
mkDagsdatoPath file date item = dagsdato </> date ++ " - " ++ location </> grade </> (name ++ " - " ++ tea) </> file
        where
            dagsdato = Dagsdato.unDagsdato $ Lens.view Main.dagsdato item
            location = takeBaseName $ Location.unLocationFile $ Lens.view Main.location item
            grade = Grade.showGrade (Lens.view Main.grades item)
            photographees = Lens.view Main.photographees item
            photographee = extract (Photographee.unPhotographees photographees)
            tea = Lens.view Photographee.tea photographee
            name = Lens.view Photographee.name photographee


mkDagsdatoBackupPath :: FilePath -> String -> Main.Item -> FilePath
mkDagsdatoBackupPath file date item = dagsdatoBackup </> date ++ " - " ++ location </> grade </> (name ++ " - " ++ tea) </> file
        where
            dagsdatoBackup = DagsdatoBackup.unDagsdatoBackup $ Lens.view Main.dagsdatoBackup item
            location = takeBaseName $ Location.unLocationFile $ Lens.view Main.location item
            grade = Grade.showGrade (Lens.view Main.grades item)
            photographees = Lens.view Main.photographees item
            photographee = extract (Photographee.unPhotographees photographees)
            tea = Lens.view Photographee.tea photographee
            name = Lens.view Photographee.name photographee


entry :: MVar FilePath -> Main.Item -> IO ()
entry mBuildFile item = do
    time <- getCurrentTime
    let date = getDate time

    let photographees = Lens.view Main.photographees item
    let photographee = extract (Photographee.unPhotographees photographees)
    myShake (opts mBuildFile photographee) date item


myShake :: ShakeOptions -> String -> Main.Item -> IO ()
myShake opts time item = shake opts $ do
    let dump = Lens.view Main.dump item
    let dumpDir = Lens.view Main.dumpDir item

    Data.List.Index.ifor_ (sort (Dump.unDumpDir dumpDir)) $ \ index' cr -> do
        let root = Dump.unDump dump
        let index = index' + 1
        let jpg = cr -<.> "jpg"

        let doneshootingCr = mkDoneshootingPath index cr item
        let doneshootingJpg = mkDoneshootingPathJpg index jpg item

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
