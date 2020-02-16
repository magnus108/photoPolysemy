module Lib.State
    ( State
    , state
    ) where


import qualified Lib.Dagsdato as Dagsdato
import qualified Lib.Shooting as Shooting
import qualified Lib.Session as Session
import qualified Lib.Photographer as Photographer
import qualified Lib.Camera as Camera


import Prelude hiding (State, state)

data State = State
--    { _dump :: Dump.State
--    , _doneshooting :: Doneshooting.State
    { _dagsdato :: Dagsdato.State
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


state ::
        Dagsdato.State
        -> Shooting.State
        -> Session.State
        -> Photographer.State
        -> Camera.State
        -> State
state = State
