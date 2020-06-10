module Lib.Build
    () where

import qualified Data.List.Index
import Data.Strings

import Utils.Comonad

import qualified Lib.Main as Main
import qualified Control.Lens as Lens

import qualified Lib.Shooting as Shooting
import qualified Lib.Photographer as Photographer
import qualified Lib.Photographee as Photographee
import qualified Lib.Session as Session
import qualified Lib.Location as Location
import qualified Lib.Grade as Grade
import qualified Lib.Dump as Dump
import qualified Lib.Doneshooting as Doneshooting
import qualified Lib.Camera as Camera

import Development.Shake
import Development.Shake.FilePath

import Data.Time.Format
import Data.Time.Clock

shakeDir :: FilePath
shakeDir = "._build"

getDate :: UTCTime -> String
getDate = formatTime defaultTimeLocale "%Y - %m%d"

opts :: ShakeOptions
opts = shakeOptions
                    { shakeFiles = shakeDir
                    , shakeProgress = progress -- should change
                    , shakeThreads = 0
                    , shakeColor = True
                    }
    where
        progress p = do
            progressDisplay 0.05 (\s -> return ()) p




mkDoneshootingPath :: Int -> FilePath -> Main.Item -> FilePath
mkDoneshootingPath index file item =
    Doneshooting.unDoneshooting doneshooting </> location </> extension </> grade </> sessionId ++ "." ++ tea ++ "." ++ shootingId ++ "." ++ photographerId ++ "." ++ no ++ (takeExtension file)
        where
            location = Location.unLocationFile $ Lens.view Main.location item
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
                
                    {-

mkDoneshootingPathJpg :: Doneshooting -> Photographee -> String -> PR.Photographer -> Session.Session -> Shooting -> String -> Int -> Camera.Camera -> FilePath
mkDoneshootingPathJpg xxx photographee location photographer session shooting filename index camera = 
    doneshooting (throw ConfigDoneshootingMissing) (\doneshootingDir -> doneshootingDir </> location </> extension
            </> "_webshop" </> sessionId ++ "." ++ tea ++ "." ++ shootingId ++ "." ++ (PR._tid photographer) ++ "." ++ (pad $ index + 1) ++ (takeExtension filename)) xxx
        where
            extension =  Camera.camera "cr3" "cr3" camera
            tea = _tea photographee
            grade = _grade photographee 
            sessionId = show $ Session.toInteger session 
            shootingId = Session.session  --- this wrongs
                    (Session.type_ (show $ toInteger shooting) ("3"))
                    (show $ toInteger shooting) session
            pad x = strPadLeft '0' 3 (show x)


mkDagsdatoPath :: Dagsdato -> Photographee -> String -> String -> UTCTime -> FilePath
mkDagsdatoPath xxx photographee location filename time =
    dagsdato (throw ConfigDagsdatoMissing) (\dagsdatoDir -> dagsdatoDir </>  ( date ++ " - " ++ location )</> grade </> (name ++ " - " ++ tea) </> filename) xxx
        where
            tea = _tea photographee
            name = _name photographee
            grade = _grade photographee 
            date = getDate time
            -}


entry :: Main.Item -> IO ()
entry item = do
    time <- getCurrentTime
    let date = getDate time
    myShake opts date item


myShake :: ShakeOptions -> String -> Main.Item -> IO ()
myShake opts time item = shake opts $ do
    let dumpDir = Lens.view Main.dumpDir item

    Data.List.Index.ifor_ (sort (Dump.unDumpDir dumpDir)) $ \ index' cr -> do
        let index = index' + 1
        let jpg = cr -<.> "jpg"

        let doneshootingCr = mkDoneshootingPath index cr item

        want [doneshootingCr] --, doneshootingJpg, dagsdatoCr, dagsdatoJpg, dagsdatoBackupCr, dagsdatoBackupJpg]

        doneshootingCr %> \f -> do
            copyFile' cr f

        {-
        doneshootingJpg %> \f -> do
            copyFile' jpg f

        dagsdatoCr %> \f -> do
            copyFile' cr f

        dagsdatoJpg %> \f -> do
            copyFile' jpg f

        dagsdatoBackupCr %> \f -> do
            copyFile' cr f

        dagsdatoBackupJpg %> \f -> do
            copyFile' jpg f
-}
            {-
            let extensionFIXME = Camera.cameras (error "fuck") (\c -> Camera.camera "cr3" "cr3" (focus c)) cameras
            let doneshootingCr = Camera.cameras (error "FUCK") (\c -> mkDoneshootingPath doneshooting photographee location photographer session shooting (takeFileName cr) index (focus c) -<.> extensionFIXME) cameras
            let doneshootingJpg = Camera.cameras (error "fuck") (\c -> mkDoneshootingPathJpg doneshooting photographee location photographer session shooting (takeFileName jpg) index (focus c) -<.> "jpg") cameras 


            --let doneshootingBackupCr2 = mkDoneshootingPath doneshootingBackup photographee location photographer session shooting (takeFileName cr3) index -<.> "cr3"
            --let doneshootingBackupJpg = mkDoneshootingPathJpg doneshootingBackup photographee location photographer session shooting (takeFileName jpg) index -<.> "jpg"

            let dagsdatoCr = mkDagsdatoPath dagsdato photographee location (takeFileName cr) time -<.> extensionFIXME
            let dagsdatoJpg = mkDagsdatoPath dagsdato photographee location (takeFileName jpg) time -<.> "jpg"

            let dagsdatoBackupCr = mkDagsdatoPath dagsdatoBackup photographee location (takeFileName cr) time -<.> extensionFIXME
            let dagsdatoBackupJpg = mkDagsdatoPath dagsdatoBackup photographee location (takeFileName jpg) time -<.> "jpg"

            want [doneshootingCr, doneshootingJpg, dagsdatoCr, dagsdatoJpg
                -- , doneshootingBackupCr2, doneshootingBackupJpg, 
                 , dagsdatoBackupCr, dagsdatoBackupJpg] 

            doneshootingCr %> \f -> do
                copyFile' cr f

            doneshootingJpg %> \f -> do
                copyFile' jpg f

            {-
            doneshootingBackupCr2 %> \f -> do
                copyFile' cr3 f

            doneshootingBackupJpg %> \f -> do
                copyFile' jpg f
            -}

            dagsdatoCr %> \f -> do
                copyFile' cr f

            dagsdatoJpg %> \f -> do
                copyFile' jpg f

            dagsdatoBackupCr %> \f -> do
                copyFile' cr f

            dagsdatoBackupJpg %> \f -> do
                copyFile' jpg f


    return ()
    -}

    {-
    files <- liftIO $ getDirectoryFilesIO dir1 ["//*.cr2"] 

    let bySys = groupBy (\x y -> toSys (splitDirectories x) == (toSys (splitDirectories y))) files

    forM_ bySys $ \ sys -> do
        ifor_ sys $ \ index' x -> do
            let index = index' + 1
            let doneshooting = toDoneshooting photographer x
            let cr2 = dir2 </> toCr2 index doneshooting
            let jpg = dir2 </> toJpg index doneshooting
            want [cr2, jpg]

            cr2 %> \f -> do 
                copyFile' (dir1 </> x) f                                                  

            jpg %> \f -> do 
                copyFile' (dir1 </> x -<.> "jpg") f                                                    



actions :: ShakeConfig -> Photographee -> String -> UTCTime -> Bool -> Rules ()
actions config photographee location time removeIt = do
        --
        -- badIO
        dagsdato <- liftIO $ getDagsdato config
        cameras <- liftIO $ getCameras config

        dump_ <- liftIO $ getDump config 
        (DumpFiles dumpFiles) <- liftIO $ getDumpFiles dump_ cameras

        dagsdatoBackup <- liftIO $ getDagsdatoBackup config
        doneshootingBackup <- liftIO $ getDoneshootingBackup config
        
        doneshooting <- liftIO $ getDoneshooting config
        photographer <- liftIO $ getPhotographer config
        session <- liftIO $ getSession config
        shooting <- liftIO $ getShooting config
        -- badIO

        ifor_ (sort dumpFiles) $ \ index (cr, jpg) -> do
            let extensionFIXME = Camera.cameras (error "fuck") (\c -> Camera.camera "cr3" "cr3" (focus c)) cameras
            let doneshootingCr = Camera.cameras (error "FUCK") (\c -> mkDoneshootingPath doneshooting photographee location photographer session shooting (takeFileName cr) index (focus c) -<.> extensionFIXME) cameras
            let doneshootingJpg = Camera.cameras (error "fuck") (\c -> mkDoneshootingPathJpg doneshooting photographee location photographer session shooting (takeFileName jpg) index (focus c) -<.> "jpg") cameras 


            --let doneshootingBackupCr2 = mkDoneshootingPath doneshootingBackup photographee location photographer session shooting (takeFileName cr3) index -<.> "cr3"
            --let doneshootingBackupJpg = mkDoneshootingPathJpg doneshootingBackup photographee location photographer session shooting (takeFileName jpg) index -<.> "jpg"

            let dagsdatoCr = mkDagsdatoPath dagsdato photographee location (takeFileName cr) time -<.> extensionFIXME
            let dagsdatoJpg = mkDagsdatoPath dagsdato photographee location (takeFileName jpg) time -<.> "jpg"

            let dagsdatoBackupCr = mkDagsdatoPath dagsdatoBackup photographee location (takeFileName cr) time -<.> extensionFIXME
            let dagsdatoBackupJpg = mkDagsdatoPath dagsdatoBackup photographee location (takeFileName jpg) time -<.> "jpg"

            want [doneshootingCr, doneshootingJpg, dagsdatoCr, dagsdatoJpg
                -- , doneshootingBackupCr2, doneshootingBackupJpg, 
                 , dagsdatoBackupCr, dagsdatoBackupJpg] 

            doneshootingCr %> \f -> do
                copyFile' cr f

            doneshootingJpg %> \f -> do
                copyFile' jpg f

            {-
            doneshootingBackupCr2 %> \f -> do
                copyFile' cr3 f

            doneshootingBackupJpg %> \f -> do
                copyFile' jpg f
            -}

            dagsdatoCr %> \f -> do
                copyFile' cr f

            dagsdatoJpg %> \f -> do
                copyFile' jpg f

            dagsdatoBackupCr %> \f -> do
                copyFile' cr f

            dagsdatoBackupJpg %> \f -> do
                copyFile' jpg f


        x <- liftIO $ getDump config
        dump (action $ return ()) (\fp -> do
                    liftIO $ setId config Id.noId
                    if removeIt then
                        action $ removeFilesAfter fp ["//*.CR3", "//*.JPG", "//*.cr3", "//*.jpg","//*.CR3","//*.cr3"]
                    else
                        return () ) x
    -}
