module Lib.Client.Main
    ( mainSection
    ) where


import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Data
import Lib.Tab
import Lib.Dump
import Lib.Client.Tab
import Lib.Client.Utils

import Lib.App (Env(..), Files(..))
import Control.Concurrent.MVar

import qualified Control.Lens as Lens


mainSection :: Env -> Window -> Tabs -> Event (Either String Dump) -> Event (Data String DumpDir) -> UI ()
mainSection env@Env{..} win tabs eDump eDumpDir = do
{-
    -- TODO this dump
    dumpDir <- liftIO $ withMVar files $ \ Files{..} -> do
        let filePath = unDump <$> dump
        dumpDir <- mapM getDumpDir filePath
        return $ join dumpDir


    (eInitial, eInitialHandle) <- liftIO newEvent

    _ <- getPhotographers mPhotographersFile eInitialHandle


    bModel <- stepper initalState $ head <$> unions'
        ((Model . Data <$> ePhotographersSucc)
            :| [ Model . Failure <$> ePhotographersErr
               , Model <$> eInitial
               , Model Loading <$ eLoading
               ])
        -}


    let (_, loading, err, succ) = splitData eDumpDir
    (eInitial, eInitialHandle) <- liftIO newEvent

    dump <- liftIO $ withMVar files $ \ Files{..} -> getDump dumpFile
    let filePath = unDump <$> dump
    -- ignore that this could be error 
    -- FIX ME....gt
    _ <- mapM (\x -> getDumpDir x eInitialHandle) filePath

    bModel <- accumB initialState $ concatenate' <$> unions'
        ((Lens.set dumpDir . Data <$> succ)
            :| [ Lens.set dumpDir . Failure <$> err
               , Lens.set dumpDir <$> eInitial
               , Lens.set dumpDir Loading <$ loading
               ])

    content <- UI.div # sink item (mkDumpDir env <$> bModel)

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    view <- UI.div #+ fmap element
        [ tabs'
        , content
        , navigation
        ]

    void $ UI.getBody win # set children [view]



mkDumpDir :: Env -> Model -> UI Element
mkDumpDir env@Env{..} model =
    case _dumpDir model of
        NotAsked -> UI.div # set text "Starting.."
        Loading -> UI.div # set text "Loading.."
        Failure _ -> do
            para <- UI.p # set text "Der er en fejl med fotografer"
            UI.div # set children [para]
        Data (DumpDir dir) -> do
            UI.div # set text (show $ length dir)
