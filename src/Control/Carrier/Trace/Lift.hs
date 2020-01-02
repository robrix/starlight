{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.Trace.Lift
( -- * Trace carrier
  TraceC(..)
  -- * Trace effect
, module Control.Effect.Trace
) where

import Control.Effect.Trace
import Control.Monad.IO.Class

newtype TraceC m a = TraceC (m a)
  deriving (Applicative, Functor, Monad, MonadIO)
