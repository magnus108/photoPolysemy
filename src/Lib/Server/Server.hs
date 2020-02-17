module Lib.Server.Server
    ( run
    ) where

import Graphics.UI.Threepenny.Core 
import qualified Graphics.UI.Threepenny as UI

import Control.Concurrent.MVar (withMVar)
import Lib.App (Env(..), Files(..))
import Lib.Doneshooting
import System.FilePath

run :: Env -> UI.Event String -> p -> IO ()
run Env{..} eAccept _ = do
    startGUI defaultConfig $ \win -> do
        bAccept <- stepper "" eAccept
        entree <- UI.entry bAccept
        _ <- element entree # set (attr "size") "10" # set style [("width","200px")]

        myButton <- UI.button # set text "Click me!"

--        value <- UI.text # set text liftIO $ do
 --           withMVar files $ \ Files{..} -> do

        UI.on UI.click myButton $ \_ -> liftIO $
            withMVar files $ \ Files{..} -> do
                (Doneshooting path) <- getDoneshooting doneshooting
                writeFile (path </> "foo.txt") "gg"

        _ <- UI.getBody win #+ [element entree, element myButton]
        return ()
