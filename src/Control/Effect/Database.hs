{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module Control.Effect.Database
( -- * Database effect
  execute
, step
, Database(..)
  -- * Re-exports
, Algebra
, HasLabelled
, run
) where

import Control.Algebra
import Control.Effect.Labelled
import Data.Text (Text)
import Database.SQLite3 (SQLData)

execute :: HasLabelled Database (Database stmt) sig m => Text -> (stmt -> m a) -> m a
execute cmd m = sendLabelled @Database (Execute cmd m)

step :: HasLabelled Database (Database stmt) sig m => stmt -> m (Maybe [SQLData])
step stmt = sendLabelled @Database (Step stmt)

data Database stmt m k where
  Execute :: Text -> (stmt -> m a) -> Database stmt m a
  Step    :: stmt                  -> Database stmt m (Maybe [SQLData])
