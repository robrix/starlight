{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Control.Effect.Thread
( -- * Thread effect
  fork
, yield
, Thread(..)
  -- * Re-exports
, Algebra
, Effect
, HasLabelled
, run
) where

import Control.Algebra
import Control.Effect.Labelled

fork :: HasLabelled Thread (Thread id) sig m => m () -> m id
fork m = sendLabelled @Thread (Labelled (Fork m pure))

yield :: HasLabelled Thread (Thread id) sig m => m ()
yield = sendLabelled @Thread (Labelled (Yield (pure ())))

data Thread id m k
  = forall a . Fork (m a) (id -> m k)
  | Yield (m k)

deriving instance Functor m => Functor (Thread id m)

instance HFunctor (Thread id) where
  hmap f = \case
    Fork m k -> Fork (f m) (f . k)
    Yield  k -> Yield      (f   k)

instance Effect (Thread id) where
  thread ctx hdl = \case
    Fork m k -> Fork (hdl (m <$ ctx)) (hdl . (<$ ctx) . k)
    Yield  k -> Yield                 (hdl (k <$ ctx))
