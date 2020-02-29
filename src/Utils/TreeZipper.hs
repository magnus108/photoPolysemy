{-# LANGUAGE DeriveAnyClass #-}

module Utils.TreeZipper
    ( TreeZipper(..)
    , Context(..)
    , mkTreeZipper
    , toRoseTree
    , toContext
    , up
    , down
    ) where

import Utils.RoseTree


data Context b l = Context [RoseTree b l] (Either b l) [RoseTree b l]
    deriving (Show, Eq, Ord, Functor)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


data TreeZipper b l = TreeZipper (RoseTree b l) [Context b l]
    deriving (Show, Eq, Ord, Functor)
    deriving (Generic)
    deriving (FromJSON, ToJSON)

toRoseTree :: TreeZipper b l -> RoseTree b l
toRoseTree (TreeZipper item _) = item

toContext :: TreeZipper b l -> [Context b l]
toContext (TreeZipper _ item) = item

mkTreeZipper :: RoseTree b l -> TreeZipper b l
mkTreeZipper x = TreeZipper x []

up :: TreeZipper b l -> Maybe (TreeZipper b l)
up (TreeZipper item (Context ls (Left x) rs:bs)) =
    Just (TreeZipper (Branch x (ls <> [item] <> rs)) bs)
up _ = Nothing


down :: (Eq b, Eq l) => Either b l -> TreeZipper b l -> Maybe (TreeZipper b l)
down x (TreeZipper (Branch parent items) bs) =
    let
        (ls, rs) = break (\item -> datum item == x) items
    in
        case rs of
            y:ys -> Just (TreeZipper y (Context ls (Left parent) ys:bs))
            _ -> Nothing
down _ _ = Nothing



{-

--fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)  
up :: TreeZipper b l -> Maybe (TreeZipper b l)
up (TreeZipper item ((Context _ x _):bs)) = Just (TreeZipper x bs)
up _ = Nothing

goToRightMostChild :: TreeZipper a -> Maybe (TreeZipper a)
goToRightMostChild (TreeZipper item@(RoseTree _ (y:ys)) xs) =
        Just (TreeZipper y (ListZipper [] item ys:xs))
goToRightMostChild _  = Nothing

--left :: TreeZipper a -> Maybe (TreeZipper a)
--left (TreeZipper item@(RoseTree x (y:ys)) xs) =
--left _  = Nothing

-}
