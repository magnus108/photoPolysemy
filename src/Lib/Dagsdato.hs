module Lib.Dagsdato
    ( State
    , state
    ) where
import Prelude hiding (State, state)


data State = State
    { _path :: FilePath
    , _backup :: FilePath
    }

state :: FilePath -> FilePath -> State
state = State
