{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Trace.Lift
( -- * Trace carrier
  runTrace
, TraceC(TraceC)
  -- * Trace effect
, module Control.Effect.Trace
) where

import Control.Algebra
import Control.Effect.Lift
import Control.Effect.Trace
import Control.Monad.IO.Class
import System.IO

newtype TraceC m a = TraceC { runTrace :: m a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance Has (Lift IO) sig m => Algebra (Trace :+: sig) (TraceC m) where
  alg = \case
    L (Trace s k) -> sendM (hPutStrLn stderr s) *> k
    R other       -> TraceC (send (handleCoercible other))
