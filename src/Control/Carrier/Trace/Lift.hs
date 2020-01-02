module Control.Carrier.Trace.Lift
( -- * Trace carrier
  TraceC(..)
) where

newtype TraceC m a = TraceC (m a)
