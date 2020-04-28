module Lib.Data
    ( Data(..)
    , splitData
    ) where

import Graphics.UI.Threepenny.Core

data Data e s
    = NotAsked
    | Loading
    | Failure e
    | Data s
        deriving Show


splitData :: Event (Data e s) -> (Event (), Event (), Event e, Event s)
splitData e = 
    (filterJust $ fromNotAsked <$> e
    , filterJust $ fromLoading <$> e
    , filterJust $ fromFailure <$> e
    , filterJust $ fromData <$> e
    )
    where
        fromLoading  Loading = Just ()
        fromLoading  _ = Nothing
        fromData (Data s) = Just s
        fromData _ = Nothing
        fromFailure (Failure e') = Just e'
        fromFailure _ = Nothing
        fromNotAsked NotAsked = Just ()
        fromNotAsked _ = Nothing

