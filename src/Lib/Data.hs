module Lib.Data
    ( Data(..)
    , splitData
    , Split(..)
    ) where

import Graphics.UI.Threepenny.Core

data Data e s
    = NotAsked
    | Loading
    | Failure e
    | Data s
        deriving Show


data Split e s = Split { notAsked :: Event ()
                          , lloading :: Event ()
                          , failure :: Event e
                          , success :: Event s
                          }


splitData :: Event (Data e s) -> Split e s
splitData e =
    let notAsked = filterJust $ fromNotAsked <$> e
        lloading = filterJust $ fromLoading <$> e
        failure = filterJust $ fromFailure <$> e
        success = filterJust $ fromData <$> e
    in
        Split{..}
    where
        fromLoading Loading = Just ()
        fromLoading _ = Nothing
        fromData (Data s) = Just s
        fromData _ = Nothing
        fromFailure (Failure e') = Just e'
        fromFailure _ = Nothing
        fromNotAsked NotAsked = Just ()
        fromNotAsked _ = Nothing

