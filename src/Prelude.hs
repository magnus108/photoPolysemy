module Prelude
    ( module Relude
    , module Json
    , module Conduit
    , readJSONFile
    ) where

import Relude
import Conduit
import Data.Conduit.Attoparsec

import Data.Aeson as Json (FromJSON (parseJSON), ToJSON (toJSON), json, Result(Error, Success), fromJSON)
import System.IO.Error


sinkFromJSON :: (MonadThrow m, FromJSON a) => ConduitM ByteString o m a
sinkFromJSON = do
    value <- sinkParser json
    case fromJSON value of
        Error e -> throwM (userError e)
        Success x -> return x


readJSONFile :: (MonadIO m, FromJSON a) => FilePath -> m a
readJSONFile fp = liftIO $ runConduitRes $ sourceFile fp .| sinkFromJSON
