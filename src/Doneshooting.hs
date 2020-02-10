{-# LANGUAGE TemplateHaskell #-}
module Doneshooting
    ( State
    , state
    , Doneshooting
    , getDoneshooting
    , _path
    ) where

import Control.Lens

data State = State
    { _path :: FilePath
    , _backup :: FilePath
    }

state :: FilePath -> FilePath -> State
state = State


-------------------------------------------------------------------------------

data Doneshooting
    = YesDoneshooting FilePath
    | NoDoneshooting
    deriving (Eq, Ord, Show)

makeLenses ''Doneshooting


getDoneshooting :: FilePath -> Doneshooting
getDoneshooting _ = NoDoneshooting
