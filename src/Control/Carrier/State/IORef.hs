{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.State.IORef
( -- * State carrier
  runState
, evalState
, StateC(..)
) where

import Control.Carrier.Reader
import Control.Monad.IO.Class.Lift
import Data.IORef

runState :: Has (Lift IO) sig m => s -> StateC s m a -> m (s, a)
runState s (StateC m) = do
  ref <- sendM (newIORef s)
  a <- runReader ref m
  s' <- sendM (readIORef ref)
  pure (s', a)

evalState :: Has (Lift IO) sig m => s -> StateC s m a -> m a
evalState s = fmap snd . runState s

newtype StateC s m a = StateC (ReaderC (IORef s) m a)
  deriving (Applicative, Functor, Monad, MonadIO)
