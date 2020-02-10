module Camera
    ( state
    , State
    ) where


newtype State = State
    { _path :: FilePath
    }

state :: FilePath -> State
state = State
