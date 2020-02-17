module Lib.App.Env
       ( Env(..)
       ) where

import Control.Concurrent.MVar

import Lib.Config
import Control.Concurrent (threadDelay)

import Lib.Dump (Dump)
import Lib.Doneshooting (Doneshooting)


data Env = Env
    { dump :: MVar Dump
    , configDump :: MVar FilePath
    , doneshooting :: MVar Doneshooting
    , configDoneshooting :: MVar FilePath
    }
