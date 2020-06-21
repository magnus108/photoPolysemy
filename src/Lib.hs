{-# LANGUAGE ScopedTypeVariables #-}
module Lib
    ( mkEnv
    , runServer
    , main
    ) where
import Lib.Data

import Relude.Unsafe (fromJust)

import qualified Graphics.UI.Threepenny as UI

import Control.Exception (SomeException(..), catch)

import System.FilePath
import Control.Concurrent.MVar (modifyMVar_)
import qualified Data.HashMap.Strict as HashMap
import System.FSNotify

import Lib.App (Env(..))
import Lib.Config (Config (..), loadConfig)
import Lib.Tab (Tabs, getTabs)
import Lib.Photographer (getPhotographers)

import Utils.ListZipper

import qualified Lib.Build as Build
import qualified Lib.Translation as Translation
import qualified Lib.Photographee as Photographee
import Lib.Dagsdato
import Lib.DagsdatoBackup
import Lib.Doneshooting
import Lib.Dump
import qualified Lib.Grade as Grade
import qualified Lib.Session as Session
import qualified Lib.Shooting as Shooting
import qualified Lib.Dump as Dump
import qualified Lib.Dagsdato as Dagsdato
import qualified Lib.Location as Location
import qualified Lib.Doneshooting as Doneshooting
import qualified Lib.DagsdatoBackup as DagsdatoBackup
import qualified Lib.Photographer as Photographer
import qualified Lib.Camera as Camera

import qualified Lib.Server.Server as Server

import Graphics.UI.Threepenny (newEvent, Handler)


mkEnv :: FilePath -> Config -> IO Env
mkEnv root' Config{..} = do
    let root = root' </> "config"
    let serverRoot = root'

    mPhotographersFile <- newMVar (root </> photographersFile)
    mGradesFile <- newMVar (root </> gradesFile)
    mDumpFile <- newMVar (root </> dumpFile)
    mDagsdatoFile <- newMVar (root </> dagsdatoFile)
    mDagsdatoBackupFile <- newMVar (root </> dagsdatoBackupFile)
    mDoneshootingFile <- newMVar (root </> doneshootingFile)
    mCamerasFile <- newMVar (root </> camerasFile)
    mShootingsFile <- newMVar (root </> shootingsFile)
    mSessionsFile <- newMVar (root </> sessionsFile)
    mLocationConfigFile <- newMVar (root </> locationConfigFile)

    mTranslationFile <- newMVar (root </> translationFile)

    mPhotographeesFile <- newMVar (root </> photograheesFile)

    mBuildFile <- newMVar (root </> buildFile)

    --ROOT'.....
    let tabs = (root' </> tabsFile)
    mTabsFile <- newMVar tabs
    pure Env{..}


runServer :: Int -> Env -> IO ()
runServer port env@Env{..} = do
    (eDirDoneshooting, hDirDoneshooting) <- newEvent
    (eConfigDoneshooting, hConfigDoneshooting) <- newEvent

    (_, hDirDagsdato) <- newEvent
    (eConfigDagsdato, hConfigDagsdato) <- newEvent

    (_, hDirDagsdatoBackup) <- newEvent
    (eConfigDagsdatoBackup, hConfigDagsdatoBackup) <- newEvent

    (eDumpDir, hDumpDir) <- newEvent
    (eConfigDump, hConfigDump) <- newEvent

    (eTabs, hTab) <- newEvent
    (ePhotographers, hPhotographers) <- newEvent

    (eCameras, hCameras) <- newEvent

    (eShootings, hShootings) <- newEvent

    (eSessions, hSessions) <- newEvent

    (eGrades, hGrades) <- newEvent
    (eLocationConfigFile, hLocationConfigFile) <- newEvent

    (ePhotographees, hPhotographees) <- newEvent

    (eBuild, hBuild) <- newEvent

    watchers <- newMVar mempty
    withManager $ \mgr -> do
        --Build
        stopBuild <- build mgr mBuildFile watchers hBuild

        --Photographers
        stopConfigPhotographers <- configPhotographers mgr mPhotographersFile watchers hPhotographers

        --Grades
        stopGrades <- grades mgr mGradesFile mLocationConfigFile mPhotographeesFile mDoneshootingFile mCamerasFile watchers hGrades hDirDoneshooting

        --Grades
        stopPhotographees <- photographees mgr mPhotographeesFile watchers hPhotographees

        --Dump
        stopConfigDump <- configDump mgr mDumpFile mCamerasFile watchers hConfigDump hDumpDir
        stopDirDump <- dirDump mgr mDumpFile mCamerasFile watchers hDumpDir

        --Dagsdato
        stopConfigDagsdato <- configDagsdato mgr mDagsdatoFile watchers hConfigDagsdato hDirDagsdato
        stopDirDagsdato <- dirDagsdato mgr mDagsdatoFile watchers hDirDagsdato

        --Dagsdato backup
        stopConfigDagsdatoBackup <- configDagsdatoBackup mgr mDagsdatoBackupFile watchers hConfigDagsdatoBackup hDirDagsdatoBackup
        stopDirDagsdatoBackup <- dirDagsdatoBackup mgr mDagsdatoBackupFile watchers hDirDagsdatoBackup

        --Doneshooting
        stopConfigDoneshooting <- configDoneshooting mgr mDoneshootingFile mCamerasFile mLocationConfigFile mGradesFile watchers hConfigDoneshooting hDirDoneshooting
        stopDirDoneshooting <- dirDoneshooting mgr mDoneshootingFile mCamerasFile mLocationConfigFile mGradesFile watchers hDirDoneshooting

        --Cameras
        stopConfigCameras <- configCameras mgr mCamerasFile mLocationConfigFile mDumpFile mDoneshootingFile mGradesFile watchers hCameras hDumpDir hDirDoneshooting

        --Shootings
        stopConfigShootings <- configShootings mgr mShootingsFile watchers hShootings

        --Sessions
        stopConfigSessions <- configSessions mgr mSessionsFile watchers hSessions

        --Location
        stopConfigLocationFile <- configLocationFile mgr mDoneshootingFile mCamerasFile mLocationConfigFile mGradesFile watchers hLocationConfigFile hDirDoneshooting

        --Tabs
        stopConfigTab <- configTab mgr mTabsFile watchers hTab

        --TODO setter
        modifyMVar_ watchers $ \_ -> do
            return $ HashMap.fromList
                [("stopConfigTab", stopConfigTab )

                ,("stopConfigLocationFile", stopConfigLocationFile)

                ,("stopBuild", stopBuild)
                ,("stopGrades", stopGrades)

                ,("stopConfigPhotographers", stopConfigPhotographers)

                ,("stopConfigCameras", stopConfigCameras)

                ,("stopConfigShootings", stopConfigShootings)

                ,("stopConfigSessions", stopConfigSessions)
                
                ,("stopConfigDoneshooting", stopConfigDoneshooting)
                ,("stopDirDoneshooting", stopDirDoneshooting)

                ,("stopConfigDagsdato", stopConfigDagsdato)
                ,("stopDirDagsdato", stopDirDagsdato)

                ,("stopConfigDagsdatoBackup", stopConfigDagsdatoBackup)
                ,("stopDirDagsdatoBackup", stopDirDagsdatoBackup)

                ,("stopConfigDump", stopConfigDump)
                ,("stopDirDump", stopDirDump)
                ,("stopPhotographees", stopPhotographees)
                ]

        --Photographers
        bBuild <- UI.stepper Build.initalState eBuild
        Build.getBuild mBuildFile >>= \case
                Left e' -> hBuild $ Build.Model (Failure (e' ++ "Kunne ikke finde byg"))
                Right s -> hBuild $ Build.Model (Data s)

        --Photographers
        bPhotographers <- UI.stepper Photographer.initalState ePhotographers
        _ <- getPhotographers mPhotographersFile >>= \case
                Left e' -> hPhotographers $ Photographer.Model (Failure e')
                Right s -> hPhotographers $ Photographer.Model (Data s)


        --Dump
        bDump <- UI.stepper Dump.initalState eConfigDump
        _ <- Dump.getDump mDumpFile >>= \case
                Left e' -> hConfigDump $ Dump.DumpModel (Failure e')
                Right s -> hConfigDump $ Dump.DumpModel (Data s)

        --Dagsdato
        bDagsdato <- UI.stepper Dagsdato.initialState eConfigDagsdato
        _ <- Dagsdato.getDagsdato mDagsdatoFile >>= \case
                Left e' -> hConfigDagsdato $ Dagsdato.Model (Failure e')
                Right s -> hConfigDagsdato $ Dagsdato.Model (Data s)

        --DagsdatoBackup
        bDagsdatoBackup <- UI.stepper DagsdatoBackup.initialState eConfigDagsdatoBackup
        _ <- DagsdatoBackup.getDagsdatoBackup mDagsdatoBackupFile  >>= \case
                Left e' -> hConfigDagsdatoBackup $ DagsdatoBackup.Model (Failure e')
                Right s -> hConfigDagsdatoBackup $ DagsdatoBackup.Model (Data s)

        -- Doneshooting
        bDoneshooting <- UI.stepper Doneshooting.initialState eConfigDoneshooting
        _ <- Doneshooting.getDoneshooting mDoneshootingFile >>= \case
            Left e' -> hConfigDoneshooting $ Doneshooting.Model (Failure e')
            Right s -> hConfigDoneshooting $ Doneshooting.Model (Data s)

        bDoneshootingDir <- UI.stepper Doneshooting.initialStateDir eDirDoneshooting
        _ <- Doneshooting.getDoneshootingDir mDoneshootingFile mCamerasFile mLocationConfigFile mGradesFile >>= \case
            Left e' -> hDirDoneshooting $ Doneshooting.DoneshootingDirModel (Failure e')
            Right s -> hDirDoneshooting $ Doneshooting.DoneshootingDirModel (Data s)


        -- Cameras
        bCameras <- UI.stepper Camera.initalState eCameras
        _ <- Camera.getCameras mCamerasFile >>= \case
            Left e' -> hCameras $ Camera.Model (Failure e')
            Right s -> hCameras $ Camera.Model (Data s)

        -- Shootings
        bShootings <- UI.stepper Shooting.initialState eShootings
        _ <- Shooting.getShootings mShootingsFile >>= \case
            Left e' -> hShootings $ Shooting.Model (Failure e')
            Right s -> hShootings $ Shooting.Model (Data s)

        -- Sessions
        bSessions <- UI.stepper Session.initialState eSessions
        _ <- Session.getSessions mSessionsFile >>= \case
            Left e' -> hSessions $ Session.Model (Failure e')
            Right s -> hSessions $ Session.Model (Data s)

        -- Grades

        bGrades <- UI.stepper Grade.initialState eGrades
        _ <- Grade.getGrades mGradesFile >>= \case
                    Left e' -> hGrades $ Grade.Model $ Failure e'
                    Right s -> hGrades $ Grade.Model $ Data s

        -- Photographees this is REAL BAD AS it does not wrk with bLocationConfigFile as expected
        bPhotographees <- UI.stepper Photographee.initialState ePhotographees
        _ <- Photographee.getPhotographees mPhotographeesFile >>= \case
            Left e' -> hPhotographees $ Photographee.Model (Failure e')
            Right s -> hPhotographees $ Photographee.Model (Data s)



        -- Location
        bLocationConfigFile <- UI.stepper Location.initialState eLocationConfigFile
        _ <- Location.getLocationFile mLocationConfigFile >>= \case
            Left e' -> hLocationConfigFile $ Location.Model (Failure e')
            Right s -> hLocationConfigFile $ Location.Model (Data s)

        -- DumpDir
        bDumpDir <- UI.stepper Dump.initalStateDir eDumpDir
        _ <- Dump.getDumpDir mDumpFile mCamerasFile >>= \case
                Left e' -> hDumpDir $ DumpDirModel (Failure e')
                Right s -> hDumpDir $ DumpDirModel (Data s)


        translations <- Translation.read mTranslationFile
        --VERY important this is here.. BADNESS FIX AT THE END
        Server.run port env (fromJust (rightToMaybe translations)) bDoneshootingDir bBuild bGrades bLocationConfigFile bSessions bShootings bCameras bDump bDumpDir bDoneshooting bDagsdato bDagsdatoBackup eTabs bPhotographers bPhotographees hGrades hLocationConfigFile hConfigDump hDumpDir hPhotographees


type WatchMap = MVar (HashMap String StopListening)


build :: WatchManager -> MVar FilePath -> WatchMap -> Handler Build.Model -> IO StopListening
build mgr mBuildFile _ handler = do
    filepath <- readMVar mBuildFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> void $ do
            print e
            Build.getBuild mBuildFile >>= \case
                Left e -> handler $ Build.Model (Failure (e ++ "Kunne ikke finde byg"))
                Right s -> handler $ Build.Model (Data s)
        )



configTab :: WatchManager -> MVar FilePath -> WatchMap -> Handler Tabs -> IO StopListening
configTab mgr mTabsFile _ handler = do
    filepath <- readMVar mTabsFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> do
            print e
            filepath' <- readMVar mTabsFile
            getTabs filepath'  >>= handler
        )


configLocationFile :: WatchManager -> MVar FilePath -> MVar FilePath -> MVar FilePath -> MVar FilePath ->  WatchMap -> Handler Location.Model -> Handler (Doneshooting.DoneshootingDirModel) -> IO StopListening
configLocationFile mgr mDoneshootingFile mCamerasFile mLocationConfigFile mGradesFile _ handler handleDonshootingDir = do
    filepath <- readMVar mLocationConfigFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> do
            --TODO madness of baddness
            print e
            filepath' <- readMVar mLocationConfigFile
            locationFile <- Location.getLocationFile' filepath'
            grades' <- mapM Photographee.parseGrades locationFile
            let grades'' = either (const (Grade.Grades (ListZipper [] (Grade.Unknown (Grade.Grade' "")) []))) id (join grades')
            _ <- Location.getLocationFile mLocationConfigFile >>= \case
                Left e' -> handler $ Location.Model (Failure e')
                Right s -> handler $ Location.Model (Data s)

            getDoneshootingDir mDoneshootingFile mCamerasFile mLocationConfigFile mGradesFile >>= \case
                Left e' -> handleDonshootingDir $ DoneshootingDirModel (Failure e')
                Right s -> handleDonshootingDir $ DoneshootingDirModel (Data s)

            void $ Grade.writeGrades mGradesFile grades''
        )`catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks


-- der skal skydes et lag in herimellem der kan lytte på locationen
photographees :: WatchManager -> MVar FilePath -> WatchMap -> Handler Photographee.Model -> IO StopListening
photographees mgr mPhotographeesFile _ handler = do
    filepath <- readMVar mPhotographeesFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> void $ do
            print e
            Photographee.getPhotographees mPhotographeesFile >>= \case
                Left e' -> handler $ Photographee.Model (Failure e')
                Right s -> handler $ Photographee.Model (Data s)
        )`catch` (\( _ :: SomeException ) -> do
            -- ikke helt nok for den skal jo også laves på ny hvis den mangler
            handler $ Photographee.Model (Failure "Der er en fejl med Photographees")
            return $ return () ) --TODO this sucks


-- der skal skydes et lag in herimellem der kan lytte på locationen
grades :: WatchManager -> MVar FilePath -> MVar FilePath -> MVar FilePath -> MVar FilePath -> MVar FilePath -> WatchMap -> Handler Grade.Model -> Handler DoneshootingDirModel -> IO StopListening
grades mgr mGradesFile mLocationConfigFile mPhotographeesFile mDoneshootingFile mCamerasFile watchMap handler handleDonshootingDir = do
    filepath <- readMVar mGradesFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> void $ do
            print e 
            print "LOLA" 
            _ <- Grade.getGrades mGradesFile >>= \case
                    Left e' -> handler $ Grade.Model $ Failure e'
                    Right s -> handler $ Grade.Model $ Data s
            Photographee.reloadPhotographees mGradesFile mLocationConfigFile mPhotographeesFile

            getDoneshootingDir mDoneshootingFile mCamerasFile mLocationConfigFile mGradesFile >>= \case
                Left e' -> handleDonshootingDir $ DoneshootingDirModel (Failure e')
                Right s -> handleDonshootingDir $ DoneshootingDirModel (Data s)

            modifyMVar_ watchMap $ \ h -> do
                h HashMap.! "stopDirDoneshooting"
                stopDirDoneshooting <- dirDoneshooting mgr mDoneshootingFile mCamerasFile mLocationConfigFile mGradesFile watchMap handleDonshootingDir
                return $ HashMap.insert "stopDirDoneshooting" stopDirDoneshooting  h

        )`catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks


configPhotographers :: WatchManager -> MVar FilePath -> WatchMap -> Handler Photographer.Model -> IO StopListening
configPhotographers mgr mFilepath _ handler = do
    filepath <- readMVar mFilepath
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        --TODO SPAWNER THREAD SOM IKKE DØR
        --TODO SPAWNER THREAD SOM IKKE DØR
        --TODO SPAWNER THREAD SOM IKKE DØR
        (\e -> void $ do
            print e 
            getPhotographers mFilepath >>= \case
                Left e' -> handler $ Photographer.Model (Failure e')
                Right s -> handler $ Photographer.Model (Data s)
        )`catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks


configSessions :: WatchManager -> MVar FilePath -> WatchMap -> Handler Session.Model -> IO StopListening
configSessions mgr mSessionsFile _ handler = do
    filepath <- readMVar mSessionsFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> do
            print e 
            Session.getSessions mSessionsFile >>= \case
                    Left e' -> handler $ Session.Model (Failure e')
                    Right s -> handler $ Session.Model (Data s)
        )`catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks


configCameras :: WatchManager -> MVar FilePath -> MVar FilePath -> MVar FilePath -> MVar FilePath -> MVar FilePath -> WatchMap -> Handler Camera.Model -> Handler (Dump.DumpDirModel) -> Handler (Doneshooting.DoneshootingDirModel) -> IO StopListening
configCameras mgr mCamerasFile mLocationConfigFile mDumpFile mDoneshootingFile mGradesFile _ handler handleDumpDir handleDonshootingDir = do
    filepath <- readMVar mCamerasFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> do
            print e
            _ <- Camera.getCameras mCamerasFile >>= \case
                Left e' -> handler $ Camera.Model (Failure e')
                Right s -> handler $ Camera.Model (Data s)

            getDumpDir mDumpFile mCamerasFile >>= \case
                Left e' -> handleDumpDir $ DumpDirModel (Failure e')
                Right s -> handleDumpDir $ DumpDirModel (Data s)

            getDoneshootingDir mDoneshootingFile mCamerasFile mLocationConfigFile mGradesFile >>= \case
                Left e' -> handleDonshootingDir $ DoneshootingDirModel (Failure e')
                Right s -> handleDonshootingDir $ DoneshootingDirModel (Data s)

        )`catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks


configShootings :: WatchManager -> MVar FilePath -> WatchMap -> Handler Shooting.Model -> IO StopListening
configShootings mgr mShootingsFile _ handler = do
    filepath <- readMVar mShootingsFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> do
            print e
            Shooting.getShootings mShootingsFile >>= \case
                Left e' -> handler $ Shooting.Model (Failure e')
                Right s -> handler $ Shooting.Model (Data s)
        )`catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks


configDoneshooting :: WatchManager -> MVar FilePath -> MVar FilePath -> MVar FilePath -> MVar FilePath -> WatchMap -> Handler Doneshooting.Model -> Handler Doneshooting.DoneshootingDirModel -> IO StopListening
configDoneshooting mgr mDoneshootingFile mCamerasFile mLocationConfigFile mGradesFile watchMap handler handleDonshootingDir = do
    filepath <- readMVar mDoneshootingFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> do
            print e
            _ <- Doneshooting.getDoneshooting mDoneshootingFile >>= \case
                Left e' -> handler $ Doneshooting.Model (Failure e')
                Right s -> handler $ Doneshooting.Model (Data s)
            -- TODO these two are related
            modifyMVar_ watchMap $ \ h -> do
                h HashMap.! "stopDirDoneshooting"
                stopDirDoneshooting <- dirDoneshooting mgr mDoneshootingFile mCamerasFile mLocationConfigFile mGradesFile watchMap handleDonshootingDir
                return $ HashMap.insert "stopDirDoneshooting" stopDirDoneshooting  h
        )`catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks



dirDoneshooting :: WatchManager -> MVar FilePath -> MVar FilePath -> MVar FilePath -> MVar FilePath -> WatchMap -> Handler Doneshooting.DoneshootingDirModel -> IO StopListening
dirDoneshooting mgr mDoneshootingFile mCamerasFile mLocationConfigFile mGradesFile _ handler = do
    doneshootingFile <- readMVar mDoneshootingFile
    doneshootingPath <- getDoneshooting' doneshootingFile
    case doneshootingPath of
        Left _ -> return ( return ())
        Right path -> do
            watchTree ---BADNESS
            ---BADNESS
            ---BADNESS
            ---BADNESS
            ---BADNESS
            ---BADNESS
                mgr
                (unDoneshooting path)
                (const True)
                (\e -> do
                    print e 
                    getDoneshootingDir mDoneshootingFile mCamerasFile mLocationConfigFile mGradesFile >>= \case
                        Left e' -> handler $ Doneshooting.DoneshootingDirModel (Failure e')
                        Right s -> handler $ Doneshooting.DoneshootingDirModel (Data s)
                ) `catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks


configDump :: WatchManager -> MVar FilePath -> MVar FilePath -> WatchMap -> Handler (Dump.DumpModel) -> Handler (Dump.DumpDirModel) -> IO StopListening
configDump mgr mDumpFile mCamerasFile watchMap handler handleDumpDir = do
    filepath <- readMVar mDumpFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        --TODO SPAWNER THREAD SOM IKKE DØR
        --TODO SPAWNER THREAD SOM IKKE DØR
        --TODO SPAWNER THREAD SOM IKKE DØR
        (\e -> do
            print e
            _ <- Dump.getDump mDumpFile >>= \case
                Left e' -> handler $ Dump.DumpModel (Failure e')
                Right s -> handler $ Dump.DumpModel (Data s)

            modifyMVar_ watchMap $ \ h -> do
                h HashMap.! "stopDirDump"
                stopDirDump <- dirDump mgr mDumpFile mCamerasFile watchMap handleDumpDir
                return $ HashMap.insert "stopDirDump" stopDirDump h
        )  `catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks


dirDump :: WatchManager -> MVar FilePath -> MVar FilePath -> WatchMap -> Handler Dump.DumpDirModel -> IO StopListening
dirDump mgr mDump mCamerasFile _ handler = do
    dumpFile <- readMVar mDump
    dumpPath <- getDump' dumpFile
    case dumpPath of
      Left _ -> return (return ()) -- TODO this sucks
      Right path -> do
        watchDir
            mgr
            (unDump path)
            (const True)
            (\e -> do
                print e
                getDumpDir mDump mCamerasFile >>= \case
                    Left e' -> handler $ Dump.DumpDirModel (Failure e')
                    Right s -> handler $ Dump.DumpDirModel (Data s)
            )`catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks


configDagsdato :: WatchManager -> MVar FilePath -> WatchMap -> Handler (Dagsdato.Model) -> Handler () -> IO StopListening
configDagsdato mgr mDagsdatoFile watchMap handler handleDagsdatoDir = do
    filepath <- readMVar mDagsdatoFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> do
            print e
            _ <- Dagsdato.getDagsdato mDagsdatoFile >>= \case
                    Left e' -> handler $ Dagsdato.Model (Failure e')
                    Right s -> handler $ Dagsdato.Model (Data s)
            modifyMVar_ watchMap $ \ h -> do
                h HashMap.! "stopDirDagsdato"
                stopDirDagsdato <- dirDagsdato mgr mDagsdatoFile watchMap handleDagsdatoDir
                return $ HashMap.insert "stopDirDagsdato" stopDirDagsdato h
        )`catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks




dirDagsdato :: WatchManager -> MVar FilePath -> WatchMap -> Handler () -> IO StopListening
dirDagsdato mgr mDagsdatoFile _ handler = do
    dagsdatoFile <- readMVar mDagsdatoFile
    dagsdatoPath <- getDagsdato' dagsdatoFile
    case dagsdatoPath of
        Left _ -> return (return ())
        Right path -> 
            watchDir
                mgr
                (unDagsdato path)
                (const True)
                (\e -> print e >> handler ())
                    `catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks


configDagsdatoBackup :: WatchManager -> MVar FilePath -> WatchMap -> Handler DagsdatoBackup.Model -> Handler () -> IO StopListening
configDagsdatoBackup mgr mDagsdatoBackupFile watchMap handler handleDagsdatoBackupDir = do
    filepath <- readMVar mDagsdatoBackupFile
    watchDir
        mgr
        (dropFileName filepath)
        (\e -> eventPath e == filepath)
        (\e -> do
            print e
            _ <- DagsdatoBackup.getDagsdatoBackup mDagsdatoBackupFile  >>= \case
                    Left e' -> handler $ DagsdatoBackup.Model (Failure e')
                    Right s -> handler $ DagsdatoBackup.Model (Data s)
            modifyMVar_ watchMap $ \ h -> do
                h HashMap.! "stopDirDagsdatoBackup"
                stopDirDagsdatoBackup <- dirDagsdatoBackup mgr mDagsdatoBackupFile watchMap handleDagsdatoBackupDir
                return $ HashMap.insert "stopDirDagsdatoBackup" stopDirDagsdatoBackup h
        )`catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks



dirDagsdatoBackup :: WatchManager -> MVar FilePath -> WatchMap -> Handler () -> IO StopListening
dirDagsdatoBackup mgr mDagsdatoBackupFile _ handler = do
    dagsdatoBackupFile <- readMVar mDagsdatoBackupFile
    dagsdatoBackupPath <- getDagsdatoBackup' dagsdatoBackupFile
    case dagsdatoBackupPath of
        Left _ -> return (return ()) -- TODO this sucks
        Right path -> watchDir
            mgr
            (unDagsdatoBackup path)
            (const True)
            (\e -> print e >> handler ())
                `catch` (\( _ :: SomeException ) -> return $ return () ) --TODO this sucks


main :: Int -> FilePath -> IO ()
main port root = do
    --traceShowM "starting"
    config' <- loadConfig (root </> "config.json")
    --traceShowM "starting still"
    mkEnv root config' >>= runServer port
