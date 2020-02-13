{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.Database.SQLite
( -- * Database carrier
  DatabaseC(..)
  -- * Database effect
, module Control.Effect.Database
) where

import           Control.Carrier.Reader
import           Control.Effect.Database
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Database.SQLite3 as SQLite

newtype DatabaseC m a = DatabaseC (ReaderC SQLite.Database m a)
  deriving (Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadTrans)
