module Lib.Client.Element
    ( mkButton
    , mkFolderPicker
    , mkFilePicker
    , control
    ) where


import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Utils.ListZipper (ListZipper)

-- condition
-- title
-- id
-- settings
-- action
-- tabs/zipper
control :: (ListZipper a -> Bool, String,String) -> ListZipper a -> (ListZipper a -> UI ()) -> UI (Maybe (Element, Element))
control (position, idd, name) items action
    | position items = return Nothing
    | otherwise = do
        (forwardButton, forwardView) <- mkButton idd name
        UI.on UI.click forwardButton $ \_ -> action items
        return (Just (forwardButton, forwardView))








mkButton :: String -> String -> UI (Element, Element)
mkButton id' x = do
    button <- UI.button # set (attr "id") id' #. "button" #+ [string x]
    view <- UI.div #. "control" #+ [element button]
    return (button, view)


mkFolderPicker :: String -> String -> (FilePath -> IO ()) -> UI (Element, Element)
mkFolderPicker = mkShowOpenDialog ["openDirectory"]


mkFilePicker :: String -> String -> (FilePath -> IO ()) -> UI (Element, Element)
mkFilePicker = mkShowOpenDialog ["openFile"]


mkShowOpenDialog :: [String] -> String -> String -> (FilePath -> IO ()) -> UI (Element, Element) 
mkShowOpenDialog options id' title' fx = do
    (button, view) <- mkButton id' title'

    UI.on UI.click button $ \_ -> do
        cb <- ffiExport fx
        runFunction $ ffi "require('electron').remote.dialog.showOpenDialog({properties: %2}, %1)" cb options

    return (button, view)
