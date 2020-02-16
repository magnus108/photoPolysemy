{-# LANGUAGE TemplateHaskell #-}
module Lib.App.Env
       ( Env(..)
       , dump
       , doneshooting
       ) where

import Control.Lens
import Lib.Dump (Dump)
import Lib.Doneshooting (Doneshooting)


data Env = Env
    { _dump :: !Dump
    , _doneshooting :: !Doneshooting
    } deriving (Show)

makeClassy ''Env
