{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.State.IORef
( -- * State carrier
  runStateRef
, runState
, evalState
, execState
, StateC(..)
  -- * State effect
, module Control.Effect.State
) where

import Control.Algebra
import Control.Carrier.Reader
import Control.Effect.State
import Control.Monad.Fix
import Control.Monad.IO.Class.Lift
import Control.Monad.Trans.Class
import Data.IORef

runStateRef :: IORef s -> StateC s m a -> m a
runStateRef ref (StateC m) = runReader ref m

runState :: forall s m a sig . Has (Lift IO) sig m => s -> StateC s m a -> m (s, a)
runState s m = do
  ref <- sendM (newIORef s)
  a <- runStateRef ref m
  s' <- sendM (readIORef ref)
  pure (s', a)

evalState :: forall s m a sig . Has (Lift IO) sig m => s -> StateC s m a -> m a
evalState s = fmap snd . runState s

execState :: forall s m a sig . Has (Lift IO) sig m => s -> StateC s m a -> m s
execState s = fmap fst . runState s

newtype StateC s m a = StateC (ReaderC (IORef s) m a)
  deriving (Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadTrans)

instance Has (Lift IO) sig m => Algebra (State s :+: sig) (StateC s m) where
  alg = \case
    L (Get   k) -> StateC ask >>= sendM . readIORef         >>= k
    L (Put s k) -> StateC ask >>= sendM . flip writeIORef s >>  k
    R other     -> StateC (send (handleCoercible other))
