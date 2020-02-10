module Parse
    ( Parse
    , fromState
    , _dump
    ) where


import qualified State
import qualified Dump

data Parse = Parse
    { _dump :: Dump.Dump
    }


fromState :: State.State -> Parse
fromState state = Parse dump
    where 
        dump = Dump.getDump $ Dump._path $ State._dump state
