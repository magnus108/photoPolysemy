{-# LANGUAGE TemplateHaskell #-}
module Lib.ControlModel
    ( mkModel
    , Model(..)
    , doneshootingDir
    , grades
    , unModel
    ) where

import           Lib.Data
import Control.Lens
import qualified Lib.Grade as Grade
import qualified Lib.Doneshooting as Doneshooting

data Item = Item
    { _grades :: Grade.Grades
    , _doneshootingDir :: Doneshooting.DoneshootingDir
    }

makeLenses ''Item

newtype Model = Model { _unModel :: Data String Item }

makeLenses ''Model

mkModel :: Grade.Model -> Doneshooting.DoneshootingDirModel -> Model
mkModel grades' doneshootingDir' =
    Model $ Item <$> Grade._grades grades' <*> (Doneshooting.unDoneshootingDirModel doneshootingDir')


