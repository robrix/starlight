{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.Trace.Lift
( -- * Trace carrier
  runTrace
, TraceC(TraceC)
  -- * Trace effect
, module Control.Effect.Trace
) where

import Control.Effect.Trace
import Control.Monad.IO.Class

newtype TraceC m a = TraceC { runTrace :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)
