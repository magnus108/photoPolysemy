module Lib.Client.Utils
    ( items
    ) where

import Graphics.UI.Threepenny.Core

items :: WriteAttr Element [UI Element]
items = mkWriteAttr $ \item container -> void $
    element container # set children [] #+ item
