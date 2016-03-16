{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Scintilla.Command.Types
    ( CommandDb(..)
    , CommandT(..)
    , runQ
    ) where

import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Reader             (MonadReader (..))
import           Control.Monad.State.Strict       (MonadState (..))
import           Control.Monad.Trans.Either       (EitherT (..), eitherT, left)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import qualified Database.PostgreSQL.Simple       as PG


class CommandDb a where
  getDb :: a -> PG.Connection


data CommandState

newtype CommandT env err evt m a = CommandT {
    unCommand :: EitherT err (ReaderT env (StateT CommandState m)) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader env, MonadState CommandState)


runQ a q = PG.query_ (getDb q) "select * from a"
