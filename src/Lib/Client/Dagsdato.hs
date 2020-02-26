module Lib.Client.Dagsdato
    ( dagsdatoSection
    ) where


import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Tab
import Lib.Dagsdato
import Lib.Client.Tab
import Lib.Client.Element

import Lib.App (Env(..), Files(..))
import Control.Concurrent.MVar (withMVar)


dagsdatoView :: Env -> Dagsdato -> UI Element
dagsdatoView Env{..} (Dagsdato dagsdato) = do
    title_ <- UI.div #+ [UI.string "Dagsdato mappe"]
    content <- UI.div #+ [UI.string dagsdato]

    picker <- UI.div #+
        [ mkFolderPicker "dagsdatoPicker" "Vælg config folder" $ \folder ->
            when (folder /= "") $
                withMVar files $ \ Files{..} ->
                    writeFile dagsdatoFile (show folder)
        ]

    UI.div #+ fmap element [ title_, content, picker]


dagsdatoSection :: Env -> Dagsdato -> Tabs -> UI Element
dagsdatoSection env@Env{..} dagsdato tabs = do

    content <- dagsdatoView env dagsdato

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    UI.div #+ fmap element
        [ tabs'
        , content
        , navigation
        ]
