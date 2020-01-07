{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Thread.IO
( runThread
, ThreadC(ThreadC)
  -- * Thread effect
, module Control.Effect.Thread
) where

import           Control.Algebra
import qualified Control.Concurrent as CC
import           Control.Effect.Lift
import           Control.Effect.Thread
import           Control.Monad (void)
import           Control.Monad.IO.Class

newtype ThreadC m a = ThreadC { runThread :: m a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance Has (Lift IO) sig m => Algebra (Thread :+: sig) (ThreadC m) where
  alg = \case
    -- NB: this discards state changes in the other thread
    L (Fork m k) -> liftWith (\ ctx run -> (<$ ctx) <$> CC.forkIO (void (run (m <$ ctx)))) >> k
    L (Yield  k) -> sendM CC.yield >> k
    R other      -> ThreadC (send (handleCoercible other))
