{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Scintilla.Query.Types
    ( MonadQuery
    , runMonadQuery
    , queryDb
    ) where

import           Control.Monad.Catch             (MonadThrow, SomeException,
                                                  throwM)
import           Control.Monad.IO.Class          (MonadIO)
import           Control.Monad.Reader            (MonadReader, ask)
import           Control.Monad.Trans.Class       (MonadTrans, lift)
import           Control.Monad.Trans.Reader      (ReaderT, runReaderT)
import qualified Data.Profunctor.Product.Default as PP
import           Data.Proxy
import           Database.PostgreSQL.Simple      (Connection)
import           GHC.Exts (Constraint)
import qualified Opaleye                         as O
import           Opaleye.SOT


type WithOnly' (p :: k) (ps :: [Perm]) = (Allow p ps
                                         , ps' ~ DropPerm '[Update, Insert, Delete, Transact, Savepoint] ps)
                                       => ps'

newtype MonadQuery m a = MonadQuery
  { unQ :: ReaderT (Conn '[ 'Fetch ]) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadThrow, MonadReader (Conn '[Fetch]))

runMonadQuery :: (Allow Fetch ps) => Conn ps -> MonadQuery m a -> m a
runMonadQuery conn = conn' . unQ
  where
    conn' = withOnly (Proxy :: Proxy '[Fetch]) conn (flip runReaderT)
class HasQuery

queryDb :: forall t m a
         . ( MonadIO m, MonadThrow m
           , PP.Default O.QueryRunner (PgR t) (HsR t)
           , Tabla t, UnHsR t a) => O.Query (PgR t) -> MonadQuery m [a]
queryDb q = do
  conn <- ask
  lift $ runQueryMany conn (unHsR (T :: T t)) q
