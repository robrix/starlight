{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.State.STM.TVar
( runStateVar
, runState
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

newtype StateC s m a = StateC (ReaderC (TVar s) m a)
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO, MonadTrans)
