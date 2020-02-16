module Lib.Camera
    ( state
    , State
    ) where

import Prelude hiding (State, state)


newtype State = State
    { _path :: FilePath
    }

state :: FilePath -> State
state = State
