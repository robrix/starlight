{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Control.Effect.Database
( -- * Database effect
  execute
, step
, Database(..)
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

execute :: HasLabelled Database (Database stmt) sig m => Text -> (stmt -> m a) -> m a
execute cmd m = sendLabelled @Database (Execute cmd m pure)

step :: HasLabelled Database (Database stmt) sig m => stmt -> m (Maybe [SQLData])
step stmt = sendLabelled @Database (Step stmt pure)

data Database stmt m k
  = forall a . Execute Text (stmt -> m a) (a -> m k)
  | Step stmt (Maybe [SQLData] -> m k)

deriving instance Functor m => Functor (Database stmt m)

instance HFunctor (Database stmt) where
  hmap f = \case
    Execute cmd m k -> Execute cmd (f . m) (f . k)
    Step stmt k -> Step stmt (f . k)

instance Effect (Database stmt) where
  thread ctx hdl = \case
    Execute cmd m k -> Execute cmd (hdl . (<$ ctx) . m) (hdl . fmap k)
    Step stmt k -> Step stmt (hdl . (<$ ctx) . k)
