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
execute cmd m = sendLabelled @Database (Labelled (Execute cmd m pure))

step :: HasLabelled Database (Database stmt) sig m => stmt -> m (Maybe [SQLData])
step stmt = sendLabelled @Database (Labelled (Step stmt pure))

data Database row m k
  = forall a . Execute Text (row -> m a) (a -> m k)
  | Step row (Maybe [SQLData] -> m k)

deriving instance Functor m => Functor (Database row m)

instance HFunctor (Database row) where
  hmap f = \case
    Execute cmd m k -> Execute cmd (f . m) (f . k)
    Step row k -> Step row (f . k)

instance Effect (Database row) where
  thread ctx hdl = \case
    Execute cmd m k -> Execute cmd (hdl . (<$ ctx) . m) (hdl . fmap k)
    Step row k -> Step row (hdl . (<$ ctx) . k)
