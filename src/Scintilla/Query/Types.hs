{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Scintilla.Query.Types
    ( DBMonad
    , runMonadQuery
    , runMonadWrite

    , queryDb
    , insertDB

    , ParseHsR(..)
    , parseHsRT
    , ToHsI(..)
    ) where

import           Control.Monad.Catch             (MonadThrow, MonadCatch, MonadMask, SomeException,
                                                  throwM, toException, Exception)
import           Control.Monad.IO.Class          (MonadIO)
import           Control.Monad.Reader            (MonadReader, ask)
import Control.Monad.Except (ExceptT, runExceptT)
import           Control.Monad.Trans.Class       (MonadTrans, lift)
import           Control.Monad.Trans.Reader      (ReaderT, runReaderT)
import qualified Data.Profunctor.Product.Default as PP
import qualified Opaleye                         as O
import           Opaleye.SOT



newtype DBMonad ps m a = DBMonad
  { unQ :: ReaderT (Conn ps) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadThrow, MonadCatch, MonadMask, MonadReader (Conn ps))


type DBWrite e ps m a = DBMonad ps (ExceptT e m) a

type QueryOnly ps = (Allow '[ 'Transact, 'Fetch] ps, Forbid 'Savepoint ps)


runMonadQuery
  :: ( MonadIO m, MonadMask m
     , QueryOnly ps, ps' ~ DropPerm ['Transact, 'Insert, 'Update, 'Delete] ps

     )
  => Conn ps -> DBMonad ps' m a -> m a
runMonadQuery conn dbm = withTransactionRead Serializable conn (runReaderT (unQ dbm))

runM :: DBWrite e ps m a -> Conn ps -> m (Either e a)
runM m conn = runExceptT . flip runReaderT conn $ unQ m

runMonadWrite
  :: ( MonadIO m, MonadMask m, Exception e
     , Allow 'Transact ps, Forbid 'Savepoint ps
     , ps' ~ ('Savepoint ': DropPerm 'Transact ps)
     )
  => Conn ps -> DBWrite e ps' m a -> m a
runMonadWrite conn dbm = do
  res <- withTransactionReadWrite Serializable conn (runM dbm)
  either (throwM . toException) return res

queryDb :: forall t ps m a
         . ( MonadIO m, MonadThrow m
           , PP.Default O.QueryRunner (PgR t) (HsR t)
           , Tabla t, ParseHsR t a, Allow 'Fetch ps)
        => O.Query (PgR t) -> DBMonad ps m [a]
queryDb q = do
  conn <- ask
  hsrs <- lift $ runQuery conn q
  traverse (either throwM return . parseHsRT (T :: T t)) hsrs


class Tabla t => ParseHsR t a where
  parseHsR :: HsR t -> Either SomeException a

instance (Tabla t, ParseHsR t a, ParseHsR t b) => ParseHsR t (a, b) where
  parseHsR hsr = (,) <$> parseHsR hsr <*> parseHsR hsr

parseHsRT :: ParseHsR t a => T t -> HsR t -> Either SomeException a
parseHsRT _ = parseHsR

class Tabla t => ToHsI t a where
  toHsI :: a -> HsI t

insertDB
  :: forall m ps w t r f proxy
   . ( MonadIO m, MonadThrow m
     , PP.Default O.QueryRunner (PgR t) (HsR t)
     , Tabla t, ToHsI t w, ParseHsR t r
     , Allow ['Insert, 'Fetch] ps, Foldable f, Functor f)
  => proxy t -> f w -> DBMonad ps m [r] -- ^
insertDB _ ws = do
  conn <- ask
  res <- lift $ runInsertReturning conn (table (T :: T t)) (pgWfromHsI . toHsI <$> ws)
  traverse (either throwM return . parseHsRT (T :: T t)) res
  -- return _res
