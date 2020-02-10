module Session
    ( State
    , state
    ) where


newtype State = State
    { _path :: FilePath
    }

state :: FilePath -> State
state = State
