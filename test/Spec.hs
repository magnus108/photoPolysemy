

import Test.Tasty
import Test.Tasty.Golden

main :: IO ()
main = do
    return ()
    {-
    _ <- getPhotographers mPhotographersFile

    _ <- Dump.getDump mDumpFile

    _ <- Dagsdato.getDagsdato mDagsdatoFile

    _ <- DagsdatoBackup.getDagsdatoBackup mDagsdatoBackupFile 

    _ <- Doneshooting.getDoneshooting mDoneshootingFile 

    _ <- Camera.getCameras mCamerasFile 

    _ <- Shooting.getShootings mShootingsFile 

    _ <- Session.getSessions mSessionsFile 

    _ <- Grade.getGrades mGradesFile 

    _ <- Photographee.getPhotographees mPhotographeesFile 

    _ <- Location.getLocationFile mLocationConfigFile 

    _ <- Dump.getDumpDir mDumpFile mCamerasFile 

    golden <- goldenTests
    defaultMain $ testGroup "tests" [ golden ]
    -}


--goldenTests :: IO TestTree
--goldenTests = do
    
    {-
    config <- toShakeConfig Nothing "test/config.cfg"    
    
    --IO bads
    dagsdatoX <- getDagsdato config
    doneshootingX <- getDoneshooting config
    
    xxxx <- getLocationFile config
    --
    --- ??????
    location (error "no location in test eeee") (\xxx -> do
            let photographeeId = "5678"
            photographee <- findPhotographee xxxx (Id.fromString photographeeId)

            let ident = _ident (fromJust  photographee)
            let goldenDir = "test" </> ident 

            -- uglys
            doneshooting (return ()) (\f -> do
                    createDirectoryIfMissing False f
                    removeDirectoryRecursive f) doneshootingX

            dagsdato (return ()) (\f -> do
                    createDirectoryIfMissing False f
                    removeDirectoryRecursive f) dagsdatoX


            let day = fromGregorian 2009 12 31
            let time = UTCTime day (secondsToDiffTime 0)

            myShake config (fromJust photographee) (takeBaseName xxx) time False

            -- bads 
            photographer <- getPhotographer config
            session <- getSession config
            shooting <- getShooting config


            -- de lader til at være en fejl at disse paths ligger her. og at null og 0 er med
            -- can throw error fixxxx
            cameras <- getCameras config

            let doneshootingPath = Camera.cameras (error "lol") (\c -> takeDirectory $ mkDoneshootingPath doneshootingX (fromJust photographee) (takeBaseName xxx) photographer session shooting "null" 0 (focus c)) cameras
            let dagsdatoPath = takeDirectory $ mkDagsdatoPath dagsdatoX (fromJust photographee) (takeBaseName xxx) "null" time

            doneShootingFiles <- listDirectory doneshootingPath
            dagsdatoFiles <- listDirectory dagsdatoPath

            -- overvej refac
            -- der er fejl i og med extension ikke er med i output
            return $ testGroup "all files moved" $ 
                [ goldenVsString
                    (takeBaseName file)
                    goldenFile
                    (LBS.readFile file)
                | file <- fmap (\x -> doneshootingPath </> x) doneShootingFiles --could be nicer
                , let goldenFile = replaceDirectory file goldenDir
                ] ++    
                [ goldenVsString
                    (takeBaseName file)
                    goldenFile
                    (LBS.readFile file)
                | file <- fmap (\x -> dagsdatoPath </> x) dagsdatoFiles --could be nicer
                , let goldenFile = replaceDirectory file goldenDir
                ]) xxxx    
                -}
