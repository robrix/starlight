{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
module Control.Carrier.Empty.Church
( -- * Empty carrier
  runEmpty
, evalEmpty
, EmptyC(EmptyC)
  -- * Empty effect
, module Control.Effect.Empty
) where

import Control.Effect.Empty
import Control.Monad (ap, void)

runEmpty :: Applicative m => EmptyC m a -> m (Maybe a)
runEmpty (EmptyC m) = m (pure . Just) (pure Nothing)

evalEmpty :: Applicative m => EmptyC m a -> m ()
evalEmpty = void . runEmpty

newtype EmptyC m a = EmptyC { runEmptyC :: forall r . (a -> m r) -> m r -> m r }
  deriving (Functor)

instance Applicative (EmptyC m) where
  pure a = EmptyC (\ just _ -> just a)
  (<*>) = ap

instance Monad (EmptyC m) where
  EmptyC m >>= f = EmptyC (\ just nothing -> m (\ a -> runEmptyC (f a) just nothing) nothing)
