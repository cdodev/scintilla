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
module Scintilla.Query.Types
    ( DBMonad
    , runMonadQuery
    , queryDb

    , ParseHsR(..)
    , parseHsRT
    ) where

import           Control.Monad.Catch             (MonadThrow, MonadCatch, MonadMask, SomeException,
                                                  throwM)
import           Control.Monad.IO.Class          (MonadIO)
import           Control.Monad.Reader            (MonadReader, ask)
import           Control.Monad.Trans.Class       (MonadTrans, lift)
import           Control.Monad.Trans.Reader      (ReaderT, runReaderT)
import qualified Data.Profunctor.Product.Default as PP
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
