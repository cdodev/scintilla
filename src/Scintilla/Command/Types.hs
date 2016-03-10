{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scintilla.Command.Types
    ( CommandDb(..)
    , CommandT(..)
    ) where

import qualified Database.PostgreSQL.Simple as PG
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import           Control.Monad.Reader      (MonadReader(..))
import Control.Monad.State.Strict (MonadState(..))
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Either (EitherT (..), eitherT,
                                              left)


class CommandDb a where
  getDb :: a -> PG.Connection


data CommandState

newtype CommandT env err evt m a = CommandT {
    unCommand :: EitherT err (ReaderT env (StateT CommandState m)) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader env, MonadState CommandState)


