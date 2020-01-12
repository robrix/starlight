{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | A 'Control.Concurrent.STM.TVar.TVar'-backed carrier for 'State'. Individual 'get's and 'put's are run 'atomically', but NB that 'modify' is /not/ atomic, so this is likely unsuitable for complex interleaving of concurrent reads and writes.
module Control.Carrier.State.STM.TVar
( runStateVar
, runState
, evalState
, execState
, StateC(..)
) where

import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Control.Monad.STM
import Control.Monad.Trans.Class

runStateVar :: TVar s -> StateC s m a -> m a
runStateVar var (StateC m) = runReader var m

runState :: Has (Lift IO) sig m => s -> StateC s m a -> m (s, a)
runState s m = do
  var <- sendM (newTVarIO s)
  a <- runStateVar var m
  s' <- sendM (atomically (readTVar var))
  pure (s', a)

evalState :: Has (Lift IO) sig m => s -> StateC s m a -> m a
evalState s = fmap snd . runState s

execState :: Has (Lift IO) sig m => s -> StateC s m a -> m s
execState s = fmap fst . runState s

newtype StateC s m a = StateC (ReaderC (TVar s) m a)
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO, MonadTrans)
