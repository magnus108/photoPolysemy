module Lib.Client.Dagsdato
    ( dagsdatoSection
    ) where


import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Tab
import Lib.Dagsdato
import Lib.Client.Tab
import Lib.Client.Element

import Lib.Client.Utils
import Lib.App (Env(..), Files(..))
import Control.Concurrent.MVar (withMVar)


dagsdatoView :: Env -> Behavior Dagsdato -> UI Element
dagsdatoView Env{..} bDagsdato = do
    title_ <- UI.div #+ [UI.string "Dagsdato mappe"]
    content <- UI.div # sink items (bDagsdato <&> \(Dagsdato dagsdato) -> [UI.string dagsdato])

    picker <- UI.div #+
            [ mkFolderPicker "dagsdatoPicker" "Vælg config folder" $ \folder ->
                when (folder /= "") $
                    withMVar files $ \ Files{..} ->
                        writeFile dagsdatoFile (show folder)
            ]

    UI.div #+ fmap element [title_, content, picker]


dagsdatoSection :: Env -> Window -> Behavior Dagsdato -> Tabs -> UI ()
dagsdatoSection env@Env{..} win bDagsdato tabs = do

    content <- dagsdatoView env bDagsdato

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    view <- UI.div #+ fmap element
        [ tabs'
        , content
        , navigation
        ]

    void $ UI.getBody win #+ fmap element [view]
