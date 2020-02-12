{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Effect.Database
( -- * Database effect
  Database(..)
) where

import Data.Text (Text)
import Database.SQLite3 (SQLData)

data Database m k
  = forall a . Execute Text ([SQLData] -> m a) (a -> m k)

deriving instance Functor m => Functor (Database m)
