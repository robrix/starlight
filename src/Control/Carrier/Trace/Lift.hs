{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Control.Monad.Fix
import Control.Monad.IO.Class
import System.IO

newtype TraceC m a = TraceC { runTrace :: m a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO)

instance Has (Lift IO) sig m => Algebra (Trace :+: sig) (TraceC m) where
  alg hdl sig ctx = case sig of
    L (Trace s) -> ctx <$ sendM (hPutStrLn stderr s)
    R other     -> TraceC (alg (runTrace . hdl) other ctx)
