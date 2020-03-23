module Lib.Client.Utils
    ( items
    , unions'
    , selectionChange'
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


items :: WriteAttr Element [UI Element]
items = mkWriteAttr $ \item container -> void $
    element container # set children [] #+ item


unions' :: NonEmpty (Event a) -> Event (NonEmpty a)
unions' = foldr (unionWith (<>)) never . fmap (fmap (:|[]))


unsafeMapUI :: Element -> (t -> UI b) -> Event t -> Event b
unsafeMapUI el f = unsafeMapIO (\a -> getWindow el >>= \w -> runUI w (f a))


selectionChange' :: Element -> Event (Maybe Int)
selectionChange' el = unsafeMapUI el (const $ UI.get UI.selection el) (domEvent "change" el)
