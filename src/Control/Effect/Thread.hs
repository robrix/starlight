{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
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
fork m = sendLabelled @Thread (Fork m pure)

kill :: HasLabelled Thread (Thread id) sig m => id -> m ()
kill i = sendLabelled @Thread (Kill i (pure ()))

yield :: HasLabelled Thread (Thread id) sig m => m ()
yield = sendLabelled @Thread (Yield (pure ()))

data Thread id m k
  = forall a . Fork (m a) (id -> m k)
  | Kill id (m k)
  | Yield (m k)

deriving instance Functor m => Functor (Thread id m)
