{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.State.IORef
( -- * State carrier
  runState
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

newtype StateC s m a = StateC (ReaderC (IORef s) m a)
  deriving (Applicative, Functor, Monad, MonadIO)
