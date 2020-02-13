{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.Database.SQLite
( -- * Database carrier
  DatabaseC(..)
) where

import           Control.Carrier.Reader
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Database.SQLite3 as SQLite

newtype DatabaseC m a = DatabaseC (ReaderC SQLite.Database m a)
  deriving (Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadTrans)
