{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

module Lib.Session
    ( Sessions(..)
    , Session(..)
    , Decisions(..)
    , getSessions
    , writeSessions
    ) where

import Utils.TreeZipper

data Session
    = KindergartenGroup
    | KindergartenSingle
    | School
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


data Decisions
    = SchoolOrKindergarten
    | GroupOrSingleForKindergarten
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


newtype Sessions = Sessions { unSessions:: TreeZipper Decisions Session }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


getSessions :: (MonadIO m, MonadThrow m) => FilePath -> m (Either String Sessions)
getSessions = readJSONFile'


writeSessions :: (MonadIO m) => FilePath -> Sessions -> m ()
writeSessions = writeJSONFile
