module Lib.Config
       ( Config (..)
       , loadConfig
       ) where

data Config = Config

loadConfig :: Monad m => m Config
loadConfig = return Config
