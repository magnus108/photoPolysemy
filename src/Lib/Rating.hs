module Lib.Rating
    ( Rating
    , fromString
    ) where

import Prelude hiding (fromString)

data Rating
    = One
    | Two
    | Three
    | Four
    | Five
    | Empty
    deriving (Show,Eq,Ord)


fromString :: String -> Rating
fromString = \case
    "1" -> One
    "2" -> Two
    "3" -> Three
    "4" -> Four
    "5" -> Five
    _ -> Empty
