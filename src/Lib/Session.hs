{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

module Lib.Session
    ( Sessions(..)
    , Session(..)
    , Decisions(..)
    , translationSessionButton
    , translationSession
    , translationDecision
    , getSessions
    , writeSessions
    , getSessions'
    , initialState
    , Model(..)
    , writeSessions'
    , toInteger
    ) where

import Prelude hiding (toInteger)
import qualified Control.Lens as Lens
import Utils.TreeZipper

import Control.Concurrent

import Lib.Data
import Control.Lens
import qualified Lib.Translation  as Translation --todo should not be here

data Session
    = KindergartenGroup
    | KindergartenSingle
    | School
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


toInteger :: Session -> Int
toInteger KindergartenSingle = 9
toInteger KindergartenGroup = 9
toInteger School = 10


data Decisions
    = SchoolOrKindergarten
    | GroupOrSingleForKindergarten
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)



--TODO this is rediculose
translationDecision :: Decisions -> Translation.Translation -> String
translationDecision decision = Lens.view translator
    where translator = case decision of
            SchoolOrKindergarten -> Translation.schoolOrKindergarten
            GroupOrSingleForKindergarten -> Translation.groupOrSingleForKindergarten


translationSessionButton :: Session -> Translation.Translation -> String
translationSessionButton session = Lens.view translator
    where translator = case session of
            KindergartenGroup -> Translation.buildGroup
            KindergartenSingle -> Translation.buildSingle
            School -> Translation.build

translationSession:: Session -> Translation.Translation -> String
translationSession session = Lens.view translator
    where translator = case session of
            KindergartenGroup -> Translation.kindergartenGroup
            KindergartenSingle -> Translation.kindergartenSingle
            School -> Translation.school


newtype Sessions = Sessions { unSessions:: TreeZipper Decisions Session }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


getSessions' :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String Sessions)
getSessions' = readJSONFile'


writeSessions' :: (MonadIO m) => FilePath -> Sessions -> m ()
writeSessions' = writeJSONFile

newtype Model = Model { unModel :: Data String Sessions }


makeLenses ''Model


initialState :: Model
initialState = Model NotAsked


--TODO could do some notification on save..
write :: (MonadIO m, MonadThrow m) => MVar FilePath -> Sessions -> m ()
write file sessions = liftIO $ withMVar file $ \f -> writeSessions' f sessions

--TODO could handle error on write.
writeSessions :: (MonadIO m) => MVar FilePath -> Sessions -> m ThreadId
writeSessions file sessions = liftIO $ forkFinally (write file sessions) $ \ _ -> return ()


read :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Sessions)
read file = liftIO $ withMVar file $ \f -> do
        --_ <- liftIO $ handle (Model Loading)
        getSessions' f


getSessions :: (MonadIO m, MonadThrow m) => MVar FilePath -> m (Either String Sessions)
getSessions file = liftIO $ read file
