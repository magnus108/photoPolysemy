module Dagsdato
    ( State
    , state
    ) where


data State = State
    { _path :: FilePath
    , _backup :: FilePath
    }

state :: FilePath -> FilePath -> State
state = State
