module Lib.Client.Main
    ( mainSection
    ) where


import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Data
import Lib.Translation
import Lib.Tab
import Lib.Grade
import Lib.Dump (Dump(..), DumpDir(..), getDump', dumpDir, _dumpDir, getDumpDir)
import qualified Lib.Dump as Dump
import Lib.Client.Tab
import Lib.Client.Utils

import Lib.App (Env(..), Files(..))
import Control.Concurrent.MVar

import qualified Control.Lens as Lens


mainSection :: Env -> Window -> Translation -> Tabs -> Event (Data String Grades) -> Event (Either String Dump) -> Event (Data String DumpDir) -> UI ()
mainSection env@Env{..} win translation tabs _ _ eDumpDir = do

    let eSplit = splitData eDumpDir
    (eInitial, eInitialHandle) <- liftIO newEvent

    dump <- liftIO $ withMVar files $ \ Files{..} -> getDump' dumpFile
    let filePath = unDump <$> dump
    -- ignore that this could be error 
    -- FIX ME....gt
    _ <- mapM (\x -> getDumpDir x eInitialHandle) filePath

    bModel <- accumB Dump.initialStateTmp $ concatenate' <$> unions'
        ((Lens.set dumpDir . Data <$> (success eSplit))
            :| [ Lens.set dumpDir . Failure <$> (failure eSplit)
               , Lens.set dumpDir <$> eInitial
               , Lens.set dumpDir Loading <$ (lloading eSplit)
               ])

    input <- UI.input #. "input" # set UI.type_ "text"
    content <- UI.div # sink item (mkDumpDir env <$> bModel)

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env translation tabs

    view <- UI.div #+ fmap element
        [ tabs'
        , content
        , input
        , navigation
        ]

    void $ UI.getBody win # set children [view]



mkDumpDir :: Env -> Dump.Model -> UI Element
mkDumpDir Env{..} model =
    case _dumpDir model of
        NotAsked -> UI.div # set text "Starting.."
        Loading -> UI.div # set text "Loading.."
        Failure _ -> do
            para <- UI.p # set text "Der er en fejl med fotografer"
            UI.div # set children [para]
        Data (DumpDir dir) -> do
            UI.div # set text (show $ length dir)
