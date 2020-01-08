{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
module Control.Carrier.Empty.CPS
( -- * Empty carrier
  runEmpty
, EmptyC(EmptyC)
  -- * Empty effect
, module Control.Effect.Empty
) where

import Control.Effect.Empty

runEmpty :: Applicative m => EmptyC m a -> m (Maybe a)
runEmpty (EmptyC run) = run (pure . Just)

newtype EmptyC m a = EmptyC (forall r . (a -> m (Maybe r)) -> m (Maybe r))
  deriving (Functor)
