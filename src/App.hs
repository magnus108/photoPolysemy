{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module App
    ( App
    ) where

import Control.Monad.Except ( ExceptT, MonadError )
import Control.Monad.Reader ( ReaderT, MonadReader )


data Config = Config
data Error = Error

newtype App m a = App
    { unApp :: ReaderT Config (ExceptT Error m) a
    } deriving
        ( Functor
        , Applicative
        , Monad
        , MonadReader Config
        , MonadError Error
        )


main :: IO ()
main = loadConfig >>= mkAppEnv >>= runServer
