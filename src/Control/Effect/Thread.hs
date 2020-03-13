{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module Control.Effect.Thread
( -- * Thread effect
  fork
, kill
, yield
, Thread(..)
  -- * Re-exports
, Algebra
, HasLabelled
, run
) where

import Control.Algebra
import Control.Effect.Labelled

fork :: HasLabelled Thread (Thread id) sig m => m () -> m id
fork m = sendLabelled @Thread (Fork m)

kill :: HasLabelled Thread (Thread id) sig m => id -> m ()
kill i = sendLabelled @Thread (Kill i)

yield :: HasLabelled Thread (Thread id) sig m => m ()
yield = sendLabelled @Thread Yield

data Thread id m k where
  Fork  :: m a -> Thread id m id
  Kill  :: id  -> Thread id m ()
  Yield ::        Thread id m ()
