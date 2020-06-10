{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Translation
    ( Translation(..)
    , doneBuild
    , building
    , noBuild
    , build
    , buildSingle
    , buildGroup
    , createPhotographee
    , dumpTab
    , dagsdatoTab
    , dagsdatoBackupTab
    , doneshootingTab
    , doneshootingBackuptab
    , photographersTab
    , cameraTab
    , shootingTab
    , sessionTab
    , locationTab
    , mainTab
    , controlTab
    , dumpDirCounter
    , photographeeName
    , photographeeIdent
    , photographeePick
    , newGrade
    , starting
    , loading
    , mainPageError
    , locationPageError
    , up
    , next
    , prev
    , reShoot
    , normal
    , cr2
    , cr3
    , dagsdatoBackupError
    , dagsdatoBackupTitle
    , dagsdatoError
    , dagsdatoTitle
    , doneshootingError
    , doneshootingTitle
    , camerasError
    , shootingsError
    , sessionsError
    , filePicker
    , folderPicker
    , photographersError
    , dumpError
    , dumpTitle
    , getTranslation'
    , read
    , writeTranslation'
    , schoolOrKindergarten
    , groupOrSingleForKindergarten
    , school
    , kindergartenSingle
    , kindergartenGroup
    , openLocation
    , newLocation
    , pickLocation
    , locationTitle
    ) where

import Control.Lens
import Control.Concurrent

data Translation = Translation { _loading :: String
                               , _build :: String
                               , _buildSingle :: String
                               , _buildGroup :: String
                               , _locationPageError :: String
                               , _mainPageError :: String
                               , _openLocation :: String
                               , _newLocation :: String
                               , _pickLocation :: String
                               , _starting :: String
                               , _filePicker :: String
                               , _folderPicker :: String
                               , _photographersError :: String
                               , _camerasError :: String
                               , _shootingsError :: String
                               , _sessionsError :: String
                               , _dumpError :: String
                               , _dumpTitle :: String
                               , _locationTitle :: String
                               , _dagsdatoError :: String
                               , _dagsdatoTitle :: String
                               , _dagsdatoBackupError :: String
                               , _dagsdatoBackupTitle :: String
                               , _doneshootingTitle :: String
                               , _doneshootingError :: String
                               , _next :: String
                               , _prev :: String
                               , _cr3 :: String
                               , _cr2 :: String
                               , _reShoot :: String
                               , _normal :: String
                               , _schoolOrKindergarten :: String
                               , _groupOrSingleForKindergarten :: String
                               , _school :: String
                               , _up :: String
                               , _kindergartenSingle :: String
                               , _kindergartenGroup :: String
                               , _newGrade :: String
                               , _dumpDirCounter :: String
                               , _createPhotographee :: String
                               , _photographeeName :: String
                               , _photographeeIdent :: String
                               , _photographeePick :: String
                               , _cameraTab :: String
                               , _dumpTab :: String
                               , _dagsdatoTab :: String
                               , _dagsdatoBackupTab :: String
                               , _doneshootingTab :: String
                               , _doneshootingBackuptab :: String
                               , _photographersTab :: String
                               , _shootingTab :: String
                               , _sessionTab :: String
                               , _locationTab :: String
                               , _mainTab :: String
                               , _controlTab :: String
                               , _doneBuild :: String
                               , _building :: String
                               , _noBuild :: String
                               }
    deriving (Show, Ord, Eq, Generic, ToJSON, FromJSON)

makeLenses ''Translation

getTranslation' :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String Translation)
getTranslation' = readJSONFile'

writeTranslation' :: (MonadIO m) => FilePath -> Translation -> m ()
writeTranslation' = writeJSONFile

read :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Translation)
read file = liftIO $ withMVar file $ \f -> do
    getTranslation' f


{-
newtype Model = Model { unModel :: Data String Photographers }


initalState :: Model
initalState = Model NotAsked


--TODO could do some notification on save..
write :: (MonadIO m, MonadThrow m) => MVar FilePath -> Photographers -> m ()
write file photographers = liftIO $ withMVar file $ \f -> writePhotographers' f photographers

--TODO could handle error on write.
writePhotographers :: (MonadIO m) => MVar FilePath -> Photographers -> m ThreadId
writePhotographers file photographers = liftIO $ forkFinally (write file photographers ) $ \ _ -> return ()


read :: (MonadIO m, MonadThrow m) => MVar FilePath -> Handler Model -> m (Either String Photographers)
read file handle = liftIO $ withMVar file $ \f -> do
        _ <- liftIO $ handle (Model Loading)
        getPhotographers' f


getPhotographers :: (MonadIO m, MonadThrow m) => MVar FilePath -> Handler Model -> m ThreadId
getPhotographers file handle = liftIO $ forkFinally (read file handle) $ \case
    Left e -> handle $ Model (Failure (show e))
    Right x -> case x of
            Left e' -> handle $ Model (Failure e')
            Right s -> handle $ Model (Data s)
            -}
