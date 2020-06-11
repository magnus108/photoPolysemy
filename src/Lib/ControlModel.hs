{-# LANGUAGE TemplateHaskell #-}
module Lib.ControlModel
    ( mkModel
    , Model(..)
    , grades
    , unModel
    ) where

import           Lib.Data
import Control.Lens
import qualified Lib.Grade as Grade

data Item = Item { _grades :: Grade.Grades }

makeLenses ''Item

newtype Model = Model { _unModel :: Data String Item }

makeLenses ''Model

mkModel :: Grade.Model -> Model
mkModel grades' =
    Model $ Item <$> Grade._grades grades'


