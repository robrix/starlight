{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | A 'Control.Concurrent.STM.TVar.TVar'-backed carrier for 'State'. Individual 'get's and 'put's are run 'atomically', but NB that 'modify' is /not/ atomic, so this is likely unsuitable for complex interleaving of concurrent reads and writes.
module Control.Carrier.State.STM.TVar
( -- * State carrier
  runStateVar
, runState
, evalState
, execState
, StateC(..)
  -- * State effect
, module Control.Effect.State
) where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Concurrent.STM.TVar
import Control.Effect.State
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.STM
import Control.Monad.Trans.Class

runStateVar :: TVar s -> StateC s m a -> m a
runStateVar var (StateC m) = runReader var m

runState :: forall s m a sig . Has (Lift IO) sig m => s -> StateC s m a -> m (s, a)
runState s m = do
  var <- sendM (newTVarIO s)
  a <- runStateVar var m
  s' <- sendM (readTVarIO var)
  pure (s', a)

evalState :: forall s m a sig . Has (Lift IO) sig m => s -> StateC s m a -> m a
evalState s = fmap snd . runState s

execState :: forall s m a sig . Has (Lift IO) sig m => s -> StateC s m a -> m s
execState s = fmap fst . runState s

newtype StateC s m a = StateC { runStateC :: ReaderC (TVar s) m a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadFix, MonadIO, MonadTrans)

instance Has (Lift IO) sig m => Algebra (State s :+: sig) (StateC s m) where
  alg ctx hdl = \case
    L (Get   k) -> StateC ask >>= sendM . readTVarIO >>= hdl . (<$ ctx) . k
    L (Put s k) -> do
      var <- StateC ask
      StateC (sendM (atomically (writeTVar var s)))
      hdl (k <$ ctx)
    R other     -> StateC (alg ctx (runStateC . hdl) (R other))
