{-# LANGUAGE DeriveAnyClass #-}
module Lib.App.Env
       ( Env(..)
       , Action(..)
       ) where


import qualified Control.Concurrent.Chan as Chan
import qualified Lib.Photographer as Photographer
import qualified Lib.Dump as Dump
import qualified Lib.Dagsdato as Dagsdato
import qualified Lib.DagsdatoBackup as DagsdatoBackup
import qualified Lib.Doneshooting as Doneshooting
import qualified Lib.Camera as Camera
import qualified Lib.Shooting as Shooting
import qualified Lib.Session as Session
import qualified Lib.Grade as Grade
import qualified Lib.Photographee as Photographee
import qualified Lib.Location as Location


data Action
    = ReadPhographers
    | WritePhographers Photographer.Photographers

    | ReadDump
    | WriteDump Dump.Dump

    | ReadDagsdato
    | WriteDagsdato Dagsdato.Dagsdato

    | ReadDagsdatoBackup
    | WriteDagsdatoBackup DagsdatoBackup.DagsdatoBackup

    | ReadDoneshooting
    | WriteDoneshooting Doneshooting.Doneshooting

    | ReadDoneshootingDir
    | WriteDoneshootingDir Doneshooting.DoneshootingDir

    | ReadCamera
    | WriteCamera Camera.Cameras

    | ReadShooting
    | WriteShooting Shooting.Shootings

    | ReadSessions
    | WriteSessions Session.Sessions

    | ReadGrades
    | WriteGrades Grade.Grades

    | ReadPhographees
    | WritePhotographees Photographee.Photographees
  
    | ReadLocation
    | WriteLocation Location.LocationFile

    | ReadDumpDir
    | WriteDumpDir Dump.DumpDir

    | SDirDagsdatoBackup
    | SDirDagsdato
    | SConfigLocationFile 
    | SGrades
    | SConfigCameras 

        deriving Show

data Env = Env
    { serverRoot :: FilePath
    , chan :: Chan.Chan Action
    , mLocationConfigFile :: MVar FilePath
    , mPhotographersFile :: MVar FilePath
    , mSessionsFile :: MVar FilePath
    , mGradesFile :: MVar FilePath
    , mCamerasFile :: MVar FilePath
    , mDumpFile :: MVar FilePath
    , mDagsdatoFile :: MVar FilePath
    , mDagsdatoBackupFile :: MVar FilePath
    , mDoneshootingFile :: MVar FilePath
    , mTranslationFile :: MVar FilePath
    , mShootingsFile :: MVar FilePath
    , mPhotographeesFile :: MVar FilePath
    , mBuildFile :: MVar FilePath
    , mTabsFile :: MVar FilePath
    }
