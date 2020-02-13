{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Database.SQLite
( -- * Database carrier
  runDatabase
, DatabaseC(..)
  -- * Database effect
, module Control.Effect.Database
) where

import           Control.Carrier.Reader
import           Control.Effect.Database
import           Control.Effect.Labelled
import           Control.Effect.Lift
import           Control.Exception.Lift
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Text (pack)
import qualified Database.SQLite3 as SQLite

runDatabase :: Has (Lift IO) sig m => FilePath -> DatabaseC m a -> m a
runDatabase file (DatabaseC m) = bracket (sendM (SQLite.open (pack file))) (sendM . SQLite.close) (`runReader` m)

newtype DatabaseC m a = DatabaseC (ReaderC SQLite.Database m a)
  deriving (Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadTrans)

instance Has (Lift IO) sig m => Algebra (Labelled Database (Database SQLite.Statement) :+: sig) (DatabaseC m) where
  alg = \case
    L (Labelled (Execute cmd m k)) -> do
      db <- DatabaseC ask
      stmt <- sendM (SQLite.prepare db cmd)
      a <- m stmt
      sendM (SQLite.finalize stmt)
      k a
    L (Labelled (Step stmt k)) -> do
      res <- sendM (SQLite.step stmt)
      case res of
        SQLite.Done -> k Nothing
        SQLite.Row  -> sendM (SQLite.columns stmt) >>= k . Just
    R other -> DatabaseC (send (handleCoercible other))
