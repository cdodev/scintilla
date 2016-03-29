{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell          #-}
module Scintilla.Command.Types
    ( CommandDb(..)
    , CommandT(..)
    , runQ
    ) where

import Control.Lens
import           Control.Applicative
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Morph              (MFunctor)
import           Control.Monad.Reader             (MonadReader (..))
import           Control.Monad.State.Strict       (MonadState (..), modify)
import           Control.Monad.Trans.Either       (EitherT (..), eitherT, left)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict (StateT(..))
import qualified Database.PostgreSQL.Simple       as PG
import qualified Data.Sequence                          as Seq
import           Data.Sequence                          (Seq)

import           Data.UUID                            (UUID)
import           Data.Thyme.Clock


class CommandDb a where
  getDb :: a -> PG.Connection

data CommandRole

data CommandEnv = CommandEnv {
    _cmdEnvRole :: CommandRole
  , _cmdEnvConnection :: PG.Connection
  }

makeLenses ''CommandEnv


data CommandEvent evt = CommandEvent {
     _eEvent :: !evt
   , _eTimestamp :: UTCTime
   , _eRole :: CommandRole
   }
newtype TxnID = TXNID { unTxnID :: UUID } deriving Show

data CommandState e = CommandState {
    _cmdEvents :: !(Seq (CommandEvent e))
  }

makeLenses ''CommandState


newtype CommandT env err evt m a = CommandT {
    unCommand :: EitherT err (ReaderT env (StateT (CommandState evt) m)) a
  } deriving (Functor, Applicative, Monad, Alternative
             , MonadIO, MonadReader env, MonadState (CommandState evt))


yieldEvent :: MonadIO m => evt -> CommandT CommandEnv err evt m ()
yieldEvent evt = do
  ts <- liftIO getCurrentTime
  r <- view cmdEnvRole
  let evt' = CommandEvent evt ts r
  modify $ cmdEvents %~ (evt' <|)


runQ a q = PG.query_ (getDb q) "select * from a"
