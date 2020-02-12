{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Effect.Database
( -- * Database effect
  Database(..)
  -- * Re-exports
, Algebra
, Effect
, HasLabelled
, run
) where

import Control.Algebra
import Control.Effect.Labelled
import Data.Text (Text)
import Database.SQLite3 (SQLData)

data Database m k
  = forall a . Execute Text ([SQLData] -> m a) (a -> m k)

deriving instance Functor m => Functor (Database m)

instance HFunctor Database where
  hmap f (Execute cmd row k) = Execute cmd (f . row) (f . k)

instance Effect Database where
  thread ctx hdl (Execute cmd row k) = Execute cmd (hdl . (<$ ctx) . row) (hdl . fmap k)
