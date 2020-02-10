module State
    ( State
    , state
    , _dump
    , _doneshooting
    ) where


import qualified Doneshooting
import qualified Dagsdato
import qualified Dump
import qualified Shooting
import qualified Session
import qualified Photographer
import qualified Camera


data State = State
    { _dump :: Dump.State
    , _doneshooting :: Doneshooting.State
    , _dagsdato :: Dagsdato.State
    , _shooting :: Shooting.State
    , _session :: Session.State
    , _photographer :: Photographer.State
    , _camera :: Camera.State
    -- these three are related
    --    -, _locationConfig :: FilePath -- this is a filter and more
    --   , _gradeConfig :: FilePath -- this is a filter and more
    --  , _photograhee :: FilePath
    -- , _buildConfig :: FilePath -- ???
    --, _stateConfig :: FilePath --Might not need guistate in this
    }


state :: Dump.State
        -> Doneshooting.State
        -> Dagsdato.State
        -> Shooting.State
        -> Session.State
        -> Photographer.State
        -> Camera.State
        -> State
state = State
