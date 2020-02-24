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
control :: (ListZipper a -> Bool, String,String) -> ListZipper a -> (ListZipper a -> UI ()) -> UI (Maybe Element)
control (position, idd, name) items action
    | position items = return Nothing
    | otherwise = do
        button <- mkButton idd name
        UI.on UI.click button $ \_ -> action items
        return (Just button)





mkButton :: String -> String -> UI Element
mkButton id' x = UI.button # set (attr "id") id' #. "button" #+ [string x]


mkFolderPicker :: String -> String -> (FilePath -> IO ()) -> UI Element
mkFolderPicker = mkShowOpenDialog ["openDirectory"]


mkFilePicker :: String -> String -> (FilePath -> IO ()) -> UI Element
mkFilePicker = mkShowOpenDialog ["openFile"]


mkShowOpenDialog :: [String] -> String -> String -> (FilePath -> IO ()) -> UI Element
mkShowOpenDialog options id' title' fx = do
    button <- mkButton id' title'

    UI.on UI.click button $ \_ -> do
        callback <- ffiExport fx
        runFunction $ ffi "require('electron').remote.dialog.showOpenDialog({properties: %2}, %1)" callback options

    return button
