{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
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
  = forall a . Execute Text (row -> m a) (a -> m k)
  | forall a . Step (row -> m a) (a -> m k)

deriving instance Functor m => Functor (Database row m)

instance HFunctor (Database row) where
  hmap f = \case
    Execute cmd row k -> Execute cmd (f . row) (f . k)
    Step        row k -> Step        (f . row) (f . k)

instance Effect (Database row) where
  thread ctx hdl = \case
    Execute cmd row k -> Execute cmd (hdl . (<$ ctx) . row) (hdl . fmap k)
    Step        row k -> Step        (hdl . (<$ ctx) . row) (hdl . fmap k)
