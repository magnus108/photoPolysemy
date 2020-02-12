{-# LANGUAGE TemplateHaskell #-}
module Doneshooting
    ( State
    , state
    , Doneshooting
    , getDoneshooting
    , _path
    , _backup
    ) where

import Control.Lens

data State = State
    { _path :: FilePath
    , _backup :: FilePath
    }

state :: FilePath -> FilePath -> State
state = State


-------------------------------------------------------------------------------

newtype Path = Path { path :: FilePath } deriving (Eq, Ord, Show)
newtype Backup = Backup { backup :: FilePath } deriving (Eq, Ord, Show)

data Doneshooting
    = YesDoneshooting Path Backup
    | NoDoneshooting
    deriving (Eq, Ord, Show)

makeLenses ''Doneshooting


getDoneshooting :: FilePath -> FilePath -> Doneshooting
getDoneshooting _ _ = NoDoneshooting
