module Lib.Client.Utils
    ( items
    , unions'
    ) where

import Graphics.UI.Threepenny.Core

items :: WriteAttr Element [UI Element]
items = mkWriteAttr $ \item container -> void $
    element container # set children [] #+ item

unions' :: NonEmpty (Event a) -> Event (NonEmpty a)
unions' = foldr (unionWith (<>)) never . fmap (fmap (:|[]))
