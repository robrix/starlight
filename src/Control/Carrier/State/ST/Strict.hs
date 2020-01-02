{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Control.Carrier.State.ST.Strict
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
import Control.Monad (ap)
import Control.Monad.ST.Strict
import Data.STRef

runStateRef :: STRef t s -> StateC s a -> ST t a
runStateRef ref (StateC m) = runReader ref m

runState :: s -> StateC s a -> (s, a)
runState s m = runST $ do
  ref <- newSTRef s
  a <- runStateRef ref m
  s' <- readSTRef ref
  pure (s', a)

evalState :: s -> StateC s a -> a
evalState s = snd . runState s

execState :: s -> StateC s a -> s
execState s = fst . runState s

newtype StateC s a = StateC (forall t . ReaderC (STRef t s) (ST t) a)
  deriving (Functor)

instance Applicative (StateC s) where
  pure a = StateC (pure a)
  (<*>) = ap

instance Monad (StateC s) where
  StateC m >>= f = StateC (m >>= (\ (StateC m) -> m) . f)

instance Algebra (State s) (StateC s) where
  alg = \case
    Get   k -> do
      s <- StateC (ReaderC (\ ref -> readSTRef ref))
      k s
    Put s k -> StateC (ReaderC (\ ref -> writeSTRef ref s)) >> k
