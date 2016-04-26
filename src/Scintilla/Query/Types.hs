{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Scintilla.Query.Types
    ( DBMonad
    , runMonadQuery
    , queryDb
    ) where

import           Control.Monad.Catch             (MonadThrow, MonadCatch, MonadMask, SomeException,
                                                  throwM)
import           Control.Monad.IO.Class          (MonadIO)
import           Control.Monad.Reader            (MonadReader, ask)
import           Control.Monad.Trans.Class       (MonadTrans, lift)
import           Control.Monad.Trans.Reader      (ReaderT, runReaderT)
import qualified Data.Profunctor.Product.Default as PP
import           Database.PostgreSQL.Simple      (Connection)
import qualified Opaleye                         as O
import           Opaleye.SOT



newtype DBMonad ps m a = DBMonad
  { unQ :: ReaderT (Conn ps) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadThrow, MonadCatch, MonadMask, MonadReader (Conn ps))

-- instance Monad m => MonadCatch (DBMonad ps m)

type QueryOnly ps = (Allow '[ 'Transact, 'Fetch] ps, Forbid 'Savepoint ps)


runMonadQuery
  :: ( MonadIO m, MonadMask m
     , QueryOnly ps, ps' ~ DropPerm ['Transact, 'Insert, 'Update, 'Delete] ps

     )
  => Conn ps -> DBMonad ps' m a -> m a
runMonadQuery conn dbm = withTransactionRead Serializable conn (runReaderT (unQ dbm))


queryDb :: forall t ps m a
         . ( MonadIO m, MonadThrow m
           , PP.Default O.QueryRunner (PgR t) (HsR t)
           , Tabla t, UnHsR t a, Allow 'Fetch ps)
        => O.Query (PgR t) -> DBMonad ps m [a]
queryDb q = do
  conn <- ask
  lift $ runQuery conn (unHsR (T :: T t)) q
