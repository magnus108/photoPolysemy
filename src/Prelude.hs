module Prelude
    ( module Relude
    , module Json
    , module Conduit
    , module Data.Conduit.Attoparsec
    , sinkFromJSON
    , (.~)
    , (^.)
    ) where

import Relude
import Conduit
import Data.Conduit.Attoparsec

import Control.Lens ((.~), (^.))

import Data.Aeson as Json (FromJSON (parseJSON), ToJSON (toJSON), json, Result(Error, Success), fromJSON)
import System.IO.Error


sinkFromJSON :: (MonadThrow m, FromJSON a) => ConduitM ByteString o m a
sinkFromJSON = do
    value <- sinkParser json
    case fromJSON value of
        Error e -> throwM (userError e)
        Success x -> return x
