{-# LANGUAGE TemplateHaskell #-}
module Dump
    ( State
    , state
    , _path
    , Dump
    , getDump
    ) where

import Control.Lens

newtype State = State
    { _path :: FilePath
    }

state :: FilePath -> State
state = State

-------------------------------------------------------------------------------

data Dump
    = YesDump FilePath
    | NoDump
    deriving (Eq, Ord, Show)

makeLenses ''Dump


getDump :: FilePath -> Dump
getDump _ = NoDump
