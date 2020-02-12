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

data Database row m k
  = Execute Text (row -> m k)

deriving instance Functor m => Functor (Database row m)

instance HFunctor (Database row) where
  hmap f (Execute cmd k) = Execute cmd (f . k)

instance Effect (Database row) where
  thread ctx hdl (Execute cmd k) = Execute cmd (hdl . (<$ ctx) . k)
