{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Utils.RoseTree
    ( RoseTree(..)
    ) where

data RoseTree a = RoseTree a [RoseTree a]
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)
    deriving (Functor, Foldable, Traversable)
