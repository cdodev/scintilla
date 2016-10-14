{-# LANGUAGE UndecidableSuperClasses #-}
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
    ( DBMonad(..)
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
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Morph (MFunctor, hoist, embed)
import           Control.Monad.Trans.Class       (MonadTrans, lift)
import           Control.Monad.Trans.Reader      (ReaderT(..), runReaderT)
import qualified Data.Profunctor.Product.Default as PP
import qualified Opaleye                         as O
import           Tisch
import           Tisch.Run


newtype DBMonad ps m a = DBMonad
  { unQ :: ReaderT (Conn ps) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadThrow, MonadCatch, MonadMask, MonadReader (Conn ps), MFunctor)


type DBWrite e ps m a = DBMonad ps (ExceptT e m) a

type QueryOnly ps = (Allow '[ 'Transact, 'Fetch] ps, Forbid 'Savepoint ps)


runMonadQuery
  :: ( MonadIO m, MonadMask m
     , QueryOnly ps, ps' ~ DropPerm ['Transact, 'Insert, 'Update, 'Delete] ps

     )
  => Conn ps -> DBMonad ps' m a -> m a
runMonadQuery conn dbm = withReadOnlyTransaction Serializable conn (runReaderT (unQ dbm))

runM :: DBWrite e ps m a -> Conn ps -> m (Either e a)
runM m conn = runExceptT . flip runReaderT conn $ unQ m

runMonadWrite
  :: ( MonadIO m, MonadMask m, Exception e
     , Allow 'Transact ps, Forbid 'Savepoint ps
     , ps' ~ ('Savepoint ': DropPerm 'Transact ps)
     )
  => Conn ps -> DBWrite e ps' m a -> m a
runMonadWrite conn dbm = do
  res <- withReadWriteTransaction Serializable conn (runM dbm)
  either (throwM . toException) return res

queryDb :: forall t ps m a
         . ( MonadIO m, MonadThrow m
           , PP.Default O.QueryRunner (PgR t) (HsR t)
           , ParseHsR t a, TableR t, Allow 'Fetch ps)
        => Query (Database t) () (PgR t) -> DBMonad ps m [a]
queryDb q = do
  conn <- ask
  hsrs <- lift $ runQuery conn q
  traverse (either throwM return . parseHsRT (undefined :: Table t)) hsrs

toWrite :: (Monad m) => DBMonad ps m a -> DBWrite e ps m a
toWrite q = hoist lift q


class TableR t => ParseHsR t a where
  parseHsR :: HsR t -> Either SomeException a

instance (TableR t, ParseHsR t a, ParseHsR t b) => ParseHsR t (a, b) where
  parseHsR hsr = (,) <$> parseHsR hsr <*> parseHsR hsr

parseHsRT :: ParseHsR t a => Table t -> HsR t -> Either SomeException a
parseHsRT _ = parseHsR

class TableR t => ToHsI t a where
  toHsI :: a -> HsI t

insertDB
  :: forall m ps w t r
   . ( MonadIO m, MonadThrow m
     , PP.Default O.QueryRunner (PgR t) (HsR t)
     , TableRW t, ToHsI t w, ParseHsR t r
     , Allow ['Insert, 'Fetch] ps)
  => Table t -> [w] -> DBMonad ps m [r] -- ^
insertDB tbl ws = do
  conn <- ask
  res <- lift $ runInsertReturning conn tbl id (toHsI <$> ws)
  traverse (either throwM return . parseHsRT tbl) res
  -- return _res


t ::  forall m ps w t r e a q
   . ( MonadIO m, MonadMask m, MonadThrow m, Exception e
     , PP.Default O.QueryRunner (PgR t) (HsR t)
     , TableR t, TableRW t, ToHsI t w, ParseHsR t r, ParseHsR t q
     , Allow ['Insert, 'Fetch] ps)
  => DBWrite e ps m [r]
t = do
  _q <- toWrite theQ
  insertDB (undefined :: Table t) (undefined :: [w])
  where
    theQ :: (MonadIO m, MonadMask m, Exception e) => DBMonad ps m [q]
    theQ = queryDb (undefined :: (Query (Database t) () (PgR t)))
