{-# LANGUAGE TemplateHaskell #-}
module Parse
    ( Parse
    , fromState
    , dump
    , doneshooting
    ) where

import Control.Lens

import qualified State
import qualified Dump
import qualified Doneshooting


data Parse = Parse
    { _dump :: Dump.Dump
    , _doneshooting :: Doneshooting.Doneshooting
    } deriving (Eq, Ord, Show)


makeLenses ''Parse


fromState :: State.State -> Parse
fromState state = Parse dump' doneshooting'
    where 
        dump' = Dump.getDump $ Dump._path $ State._dump state
        doneshooting' = Doneshooting.getDoneshooting
                (Doneshooting._path (State._doneshooting state))
                (Doneshooting._backup (State._doneshooting state))
