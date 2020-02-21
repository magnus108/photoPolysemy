module Lib.Client.Doneshooting
    ( doneshootingSection
    ) where


import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Lib.Tab
import Lib.Doneshooting
import Lib.Client.Tab
import Lib.Client.Element

import Lib.App (Env(..))






--TODO move me
items :: WriteAttr Element (UI Element)
items = mkWriteAttr $ \item container -> void $
    element container # set children [] #+ [item]



doneshootingView :: Env -> Doneshooting -> UI Element
doneshootingView _ (Doneshooting doneshooting) = do
    title' <- UI.string "Dump mappe"
    content <- UI.string doneshooting
    (_, pickerView) <- mkFolderPicker "dumpPicker" "Vælg config folder" $ \folder -> 
        when (folder /= "") $ do
            liftIO $ putStrLn "lol"

    UI.div #+ fmap element [pickerView, title', content]



doneshootingSection :: Env -> Behavior Doneshooting -> Tabs -> UI Element
doneshootingSection env@Env{..} bDoneshooting tabs = do

    --(_, pickerView) <- mkFolderPicker "dumpPicker" "Vælg config folder" $ \folder -> 
     --   when (folder /= "") $ do
      --      liftIO $ putStrLn "lol"



    
    {-
    --with <- mkSection [ mkColumns ["is-multiline"]
     --                       [ mkColumn ["is-12"] [ mkLabel "Dump mappe ikke valgt" # set (attr "id") "dumpMissing" ]
      --                      , mkColumn ["is-12"] [ element picker ]
       --                     ]
        --              ] 
    --view <- Dump.dump (element with) without dump


    let without = (\z -> mkSection [ mkColumns ["is-multiline"]
                            [ mkColumn ["is-12"] [ mkLabel "Dump mappe" # set (attr "id") "dumpOK" ]
                            , mkColumn ["is-12"] [ element picker ]
                            , mkColumn ["is-12"] [ UI.p # set UI.text z # set (attr "id") "dumpPath" ]
                            --, mkColumn ["is-12"] [ element forwardView ]
                            ]
                      ])
   -}

    content <- UI.div # sink items (fmap (doneshootingView env) bDoneshooting)

    tabs' <- mkTabs env tabs
    next' <- next env tabs
    prev' <- prev env tabs

    UI.div #+ fmap element (
        [ tabs'
        , content
        ] 
        ++ maybeToList (fmap snd next')
        ++ maybeToList (fmap snd prev')
        )

