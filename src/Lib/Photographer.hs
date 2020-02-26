{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

module Lib.Photographer
    ( Photographer(..)
    , Photographers(..)
    , getPhotographers
    , writePhotographers
    ) where

import Utils.ListZipper

type Name = String
type Tid = String

data Photographer = Photographer
    { _name :: Name
    , _tid :: Tid
    --TODO --ord?
    } deriving (Show, Ord, Eq, Generic, ToJSON, FromJSON)


newtype Photographers = Photographers { unPhotographers :: ListZipper Photographer }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


getPhotographers :: (MonadIO m, MonadThrow m) => FilePath -> m Photographers
getPhotographers = readJSONFile


writePhotographers :: (MonadIO m) => FilePath -> Photographers -> m ()
writePhotographers = writeJSONFile
