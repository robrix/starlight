{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
module Control.Carrier.Empty.Church
( EmptyC(EmptyC)
) where

newtype EmptyC m a = EmptyC (forall r . (a -> m r) -> m r -> m r)
  deriving (Functor)
