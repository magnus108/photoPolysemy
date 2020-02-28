module Utils.TreeZipper
    ( TreeZipper(..)
    , mkTreeZipper
    , up
    , goToRightMostChild
    ) where

import Utils.RoseTree
import Utils.ListZipper

--import Utils.Comonad

data TreeZipper a = TreeZipper (RoseTree a) [ListZipper (RoseTree a)]
        deriving (Show, Eq, Functor)


mkTreeZipper :: RoseTree a -> TreeZipper a
mkTreeZipper a = TreeZipper a []


up :: TreeZipper a -> Maybe (TreeZipper a)
up (TreeZipper _ ((ListZipper _ x _):bs)) = Just (TreeZipper x bs)
up _ = Nothing


goToRightMostChild :: TreeZipper a -> Maybe (TreeZipper a)
goToRightMostChild (TreeZipper item@(RoseTree _ (y:ys)) xs) =
        Just (TreeZipper y (ListZipper [] item ys:xs))
goToRightMostChild _  = Nothing

--left :: TreeZipper a -> Maybe (TreeZipper a)
--left (TreeZipper item@(RoseTree x (y:ys)) xs) =
--left _  = Nothing


    {-
instance Comonad TreeZipper where
    extract (TreeZipper a _) = a
    duplicate (TreeZipper a as) = fmap dublicate as
    -}
       -- ListZipper (shift backward') a (shift forward')
        --where shift move = tail $ iterate' move a
