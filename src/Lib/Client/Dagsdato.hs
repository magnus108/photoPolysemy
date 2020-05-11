module Lib.Client.Dagsdato
    ( dagsdatoSection
    ) where


import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Translation
import Lib.Tab
import Lib.Dagsdato
import Lib.Client.Tab
import Lib.Client.Element

import Lib.Client.Utils
import Lib.App (Env(..), Files(..))
import Control.Concurrent.MVar (withMVar)


dagsdatoView :: Env -> Either String Dagsdato -> UI Element
dagsdatoView Env{..} = \case
    Left _ -> do
        title_ <- UI.p # set text "Der er en fejl med dagsdato mappe"

        picker <- UI.div #+
                [ mkFolderPicker "dagsdatoPicker" "Vælg config folder" $ \folder ->
                    when (folder /= "") $
                        withMVar files $ \ Files{..} ->
                            writeFile dagsdatoFile (show folder)
                ]

        UI.div #+ fmap element [title_, picker]
    Right dagsdato -> do
        title_ <- UI.div #+ [UI.string "Dagsdato mappe"]
        content <- UI.div #+ [UI.string (unDagsdato dagsdato)]

        picker <- UI.div #+
                [ mkFolderPicker "dagsdatoPicker" "Vælg config folder" $ \folder ->
                    when (folder /= "") $
                        withMVar files $ \ Files{..} ->
                            writeFile dagsdatoFile (show folder)
                ]

        UI.div #+ fmap element [title_, content, picker]


dagsdatoSection :: Env -> Window -> Translation -> Event (Either String Dagsdato) -> Tabs -> UI ()
dagsdatoSection env@Env{..} win translation eDagsdato tabs = do
    dagsdato <- liftIO $ withMVar files $ \ Files{..} -> getDagsdato dagsdatoFile
    bDagsdato <- stepper dagsdato eDagsdato

    content <- UI.div # sink item (dagsdatoView env <$> bDagsdato)

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env translation tabs

    view <- UI.div #+ fmap element
        [ tabs'
        , content
        , navigation
        ]

    void $ UI.getBody win # set children [view]
