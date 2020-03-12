module Lib.Client.Location
    ( locationSection
    ) where

import qualified Data.Aeson                  as JSON

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Utils.Comonad
import qualified Utils.ListZipper as ListZipper

import Lib.Grade
import Lib.Tab
import Lib.Location
import Lib.Client.Tab

import Lib.Client.Element
import Lib.App (Env(..), Files(..))
import Control.Concurrent.MVar (withMVar)


locationFileView :: Env -> LocationFile -> UI Element
locationFileView Env{..} locationFile = do
    title_ <- UI.div #+ [UI.string "Lokation"]
    content <- UI.div #+ [UI.string (unLocationFile locationFile)]

    pick <- mkFilePicker "locationFilePicker" "Vælg lokations" $ \file ->
            when (file /= "") $
                withMVar files $ \ Files{..} ->
                    writeLocationFile locationConfigFile (LocationFile file)

    make <- mkFileMaker "locationsPicker" "Ny CSV" $ \file ->
            when (file /= "") $
                withMVar files $ \ Files{..} ->
                    writeLocationFile locationConfigFile (LocationFile file)

    pickers <- UI.div #. "buttons has-addons" #+ [element pick, element make]

    open <- mkOpenFile "open" "Åben csv" (unLocationFile locationFile)

    UI.div #+ fmap element [ title_, content, pickers, open ]


mkGrades :: Env -> LocationFile -> Grades -> UI Element
mkGrades env locationFile grades = do
    grades' <- mapM (mkGrade env locationFile) elems
    UI.select #+ fmap element (ListZipper.toList grades')
        where
            elems = unGrades grades =>> \grades'' ->
                    (extract grades'', Grades grades'')


mkGrade :: Env -> LocationFile -> (Grade, Grades) -> UI Element
mkGrade _ _ (grade, grades) = do
    let name = show grade
    UI.option # set (attr "value") name  # set text name



gradesView :: Env -> LocationFile -> Grades -> UI Element
gradesView env@Env{..} locationFile grades = do
    gradeInsert <- mkButton "insert" "Tilføj ny"

    UI.on UI.click gradeInsert $ \_ ->
        liftIO $ withMVar files $ \ Files{..} ->
            --TODO fix this up
            writeGrades gradesFile $ Grades $ ListZipper.insert (unGrades grades) (Grade "")

    _ <- mkButton "delete" "Slet"

    gradesView <- mkGrades env locationFile grades

    UI.on UI.selectionChange gradesView$ \i -> do
        liftIO $ putStrLn (show i)


    UI.div #+ fmap element [ gradeInsert, gradesView]














    {-
            gradesView <- Grade.grades (UI.div #+ [UI.div #. "field" #+
                                [ UI.label #. "label has-text-dark" # set UI.text "Ingen stuer/klasser"
                                , UI.div # set (attr "style") "width:100%" #. "select" #+ 
                                        [ UI.select # set (attr "disabled") "true" # set (attr "style") "width:100%" #+ []
                                        ]
                                ]
                                , UI.div #. "control" #+ [element gradeInsertView']
                                ])

                        (\(ListZipper.ListZipper ls y rs) -> do
                                    -- i dont need this if i just make sure the
                                    -- list is sorted on the type level
                                    --let zipper = Zipper.ListZipper (reverse (sort ls)) y (sort rs)

                                    let zipper = ListZipper.sorted (ls ++ rs) (ListZipper.ListZipper [] y [])

                                    input <- UI.select # set (attr "style") "width:100%" # set (attr "id") "inputter"
                                    
                                    --hack create extendI
                                    gradeViews <- sequence $ ListZipper.iextend (\ i z -> do
                                                        opt <- UI.option # set (attr "value") (extract z) # set (attr "id") (extract z) # set text (extract z)
                                                        opt' <- if (z == zipper) then
                                                                element opt # set (UI.attr "selected") "" # set (UI.attr "id") "selected"
                                                            else
                                                                return opt
 
                                                        let name = ("t" ++ (show i))
                                                        runFunction $ ffi "new CustomEvent(%1,{})" name
                                                        onEvent (domEvent name input) $ \x -> do
                                                                liftIO $ Chan.writeChan msgs $ Msg.setGrades $ Grade.yesGrades z 

                                                        return opt
                                                    ) zipper

                                    _ <- element input # set children (ListZipper.toList gradeViews)

                                    on UI.selectionChange input $ \ i -> do
                                        case i of
                                            Nothing -> return ()
                                            Just n -> when (length ls /= n) $ do
                                                runFunction $ ffi "$('#inputter').trigger(%1)" ("t"++(show n))

                                    (gradeInsert, gradeInsertView) <- mkButton "insert" "Tilføj ny klasse"

                                    on UI.click gradeInsert $ \_ -> do 
                                            liftIO $ Chan.writeChan msgs $ Msg.setGrades $ Grade.yesGrades $ ListZipper.insert zipper mempty 

                                    (gradeDelete, gradeDeletetView) <- mkButton "delete" "Slet alle klasser"

                                    on UI.click gradeDelete $ \_ -> do 
                                            liftIO $ Chan.writeChan msgs $ Msg.setGrades $ Grade.noGrades 
                                    

                                    inputView <- UI.div #. "field" #+
                                        [ UI.div # set (attr "style") "width:100%" #. "select" #+ [ element input ] 
                                        ]


                                    input2 <- UI.input #. "input" # set (attr "id") "focusGrade" #  set UI.type_ "text" # set (attr "value") (extract zipper)

                                    inputView2 <- UI.div #. "field" #+
                                        [ UI.label #. "label has-text-dark" # set UI.text "Ændre valgte"
                                        , UI.div #. "control" #+ [ element input2 ] 
                                        ]

                                    (gradeChange, gradeChangeView) <- mkButton "save" "Gem"

                                    (gradeDeleteSingle, gradeDeleteSingleView) <- mkButton "delete" "Delete"

                                    on UI.click gradeDeleteSingle $ \_ -> do 
                                            liftIO $ Chan.writeChan msgs $ Msg.setGrades $ Grade.delete (Grade.yesGrades zipper)

                                    on UI.keydown inputView2 $ \keycode -> when (keycode == 13) $ do
                                        UI.setFocus gradeChange
                                        runFunction $ ffi "$('#save').trigger('click')"
                                        return ()

                                    on UI.click gradeChange $ \_ -> do
                                        val <- get value input2
                                        -- simpel sortering vil ikke virke her
                                        liftIO $ Chan.writeChan msgs $ Msg.setGrades $ Grade.yesGrades $ ListZipper.mapFocus (\focus -> val) zipper


                                    UI.div #. "field" #+
                                        [ UI.label #. "label has-text-dark" # set UI.text "Klasse/stue"
                                        , UI.div #. "control" #+ [ UI.div #. "buttons has-addons" #+ [element gradeInsert, element gradeDelete] , element inputView]
                                        , UI.br --bads
                                        , UI.div #. "control" #+ [ element inputView2, UI.div #. "buttons has-addons" #+ [element gradeChange, element gradeDeleteSingle]]
                                        ]



                            ) grades
                            -}


locationSection :: Env -> LocationFile -> Grades -> Tabs -> UI Element
locationSection env@Env{..} locationFile grades tabs = do
    content <- locationFileView env locationFile
    gradesContent <- gradesView env locationFile grades

    tabs' <- mkTabs env tabs
    navigation <- mkNavigation env tabs

    UI.div #+ fmap element
        [ tabs'
        , content
        , gradesContent
        , navigation
        ]
