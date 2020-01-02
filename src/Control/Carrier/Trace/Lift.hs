module Control.Carrier.Trace.Lift
( -- * Trace carrier
  TraceC(..)
  -- * Trace effect
, module Control.Effect.Trace
) where

import Control.Effect.Trace

newtype TraceC m a = TraceC (m a)
