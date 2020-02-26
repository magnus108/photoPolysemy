{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

module Lib.Tab
    ( Tabs(..)
    , Tab(..)
    , getTabs
    , writeTabs
    ) where

import Utils.ListZipper

data Tab
    = DumpTab
    | DagsdatoTab
    | DagsdatoBackupTab
    | DoneshootingTab
    | DoneshootingBackupTab
    | PhotographersTab
    | CameraTab
    | ShootingTab
    | SessionTab
    | LocationTab
    | MainTab
    | ControlTab
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)


newtype Tabs = Tabs { unTabs :: ListZipper Tab }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving (FromJSON, ToJSON)



getTabs :: (MonadIO m, MonadThrow m) => FilePath -> m Tabs
getTabs = readJSONFile


writeTabs :: (MonadIO m) => FilePath -> Tabs -> m ()
writeTabs = writeJSONFile
