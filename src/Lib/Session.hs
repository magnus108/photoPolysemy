module Lib.Session
    ( State
    , state
    ) where
import Prelude hiding (State, state)


newtype State = State
    { _path :: FilePath
    }

state :: FilePath -> State
state = State
