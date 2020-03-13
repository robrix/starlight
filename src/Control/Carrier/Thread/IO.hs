{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import           Control.Effect.Labelled
import           Control.Effect.Lift
import           Control.Effect.Thread
import           Control.Monad (void)
import           Control.Monad.Fix
import           Control.Monad.IO.Class

newtype ThreadC m a = ThreadC { runThread :: m a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO)

instance Has (Lift IO) sig m => Algebra (Labelled Thread (Thread CC.ThreadId) :+: sig) (ThreadC m) where
  alg hdl sig ctx = case sig of
    -- NB: this discards state changes in the other thread
    L (Labelled (Fork m k)) -> liftWith (\ hdl2 ctx2 -> (<$ ctx2) . (<$ ctx) <$> CC.forkIO (void (hdl2 (hdl (m <$ ctx) <$ ctx2)))) >>= hdl . fmap k
    L (Labelled (Kill i k)) -> sendM (CC.killThread i) >> hdl (k <$ ctx)
    L (Labelled (Yield  k)) -> sendM CC.yield >> hdl (k <$ ctx)
    R other                 -> ThreadC (alg (runThread . hdl) other ctx)
