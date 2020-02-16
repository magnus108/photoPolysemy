{-# LANGUAGE TemplateHaskell #-}
module Lib.Dump
    ( Dump
    , getDump
    ) where

import Control.Lens

data Dump
    = YesDump FilePath
    | NoDump
    deriving (Eq, Ord, Show)

makeLenses ''Dump


getDump :: FilePath -> Dump
getDump _ = NoDump
