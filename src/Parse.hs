{-# LANGUAGE TemplateHaskell #-}
module Parse
    ( Parse
    , fromState
    , dump
    ) where

import Control.Lens

import qualified State
import qualified Dump


data Parse = Parse
    { _dump :: Dump.Dump
    } deriving (Eq, Ord, Show)


makeLenses ''Parse


fromState :: State.State -> Parse
fromState state = Parse dump'
    where 
        dump' = Dump.getDump $ Dump._path $ State._dump state
