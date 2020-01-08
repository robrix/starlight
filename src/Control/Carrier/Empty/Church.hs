{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
module Control.Carrier.Empty.Church
( EmptyC(EmptyC)
) where

import Control.Monad (ap)

newtype EmptyC m a = EmptyC { runEmptyC :: forall r . (a -> m r) -> m r -> m r }
  deriving (Functor)

instance Applicative (EmptyC m) where
  pure a = EmptyC (\ just _ -> just a)
  (<*>) = ap

instance Monad (EmptyC m) where
  EmptyC m >>= f = EmptyC (\ just nothing -> m (\ a -> runEmptyC (f a) just nothing) nothing)
