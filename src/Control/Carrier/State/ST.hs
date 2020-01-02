{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Control.Carrier.State.ST
( -- * State carrier
  StateC(..)
  -- * State effect
, module Control.Effect.State
) where

import Control.Algebra
import Control.Carrier.Reader
import Control.Effect.State
import Control.Monad (ap)
import Control.Monad.ST
import Data.STRef

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
