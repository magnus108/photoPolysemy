module Lib.Client.Dump
    ( dumpSection
    ) where


import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Tab
import Lib.Client.Tab

import Lib.App (Env(..))


import Lib.Client.Element


dumpSection :: Env -> Tabs -> UI Element
dumpSection env@Env{..} tabs = do
    

    pickerView <- mkFolderPicker "dumpPicker" "Vælg config folder" $ \folder -> 
        when (folder /= "") $ do
            liftIO $ putStrLn "lol"

    --with <- mkSection [ mkColumns ["is-multiline"]
     --                       [ mkColumn ["is-12"] [ mkLabel "Dump mappe ikke valgt" # set (attr "id") "dumpMissing" ]
      --                      , mkColumn ["is-12"] [ element picker ]
       --                     ]
        --              ] 
    {-


    {-
    let without = (\z -> mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Dump mappe" # set (attr "id") "dumpOK" ]
                            , mkColumn ["is-12"] [ element picker ]
                            , mkColumn ["is-12"] [ UI.p # set UI.text z # set (attr "id") "dumpPath" ]
                            --, mkColumn ["is-12"] [ element forwardView ]
                            ]
                      ])
                      -}
    --view <- Dump.dump (element with) without dump

-}

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    UI.div #+ fmap element (
        [ pickerView
        , tabs'
        , navigation
        ]
        )

