module Dump
    ( State
    , state
    , _path
    , Dump
    , getDump
    ) where


newtype State = State
    { _path :: FilePath
    }

state :: FilePath -> State
state = State

-------------------------------------------------------------------------------

data Dump
    = YesDump FilePath
    | NoDump


getDump :: FilePath -> Dump
getDump f = NoDump
