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
import Control.Monad (ap)
import Control.Monad.Trans.Class

runEmpty :: Applicative m => EmptyC m a -> m (Maybe a)
runEmpty (EmptyC run) = run (pure . Just)

newtype EmptyC m a = EmptyC { runEmptyC :: forall r . (a -> m (Maybe r)) -> m (Maybe r) }
  deriving (Functor)

instance Applicative (EmptyC m) where
  pure a = EmptyC ($ a)
  (<*>) = ap

instance Monad (EmptyC m) where
  EmptyC m >>= f = EmptyC (\ k -> m (($ k) . runEmptyC . f))

instance MonadFail m => MonadFail (EmptyC m) where
  fail = lift . fail

instance MonadTrans EmptyC where
  lift m = EmptyC (m >>=)
