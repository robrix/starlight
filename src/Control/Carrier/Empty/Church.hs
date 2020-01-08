{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
module Control.Carrier.Empty.Church
( -- * Empty carrier
  runEmpty
, evalEmpty
, execEmpty
, EmptyC(EmptyC)
  -- * Empty effect
, module Control.Effect.Empty
) where

import Control.Effect.Empty
import Control.Monad (ap, void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Maybe (isJust)

runEmpty :: Applicative m => EmptyC m a -> m (Maybe a)
runEmpty (EmptyC m) = m (pure . Just) (pure Nothing)

evalEmpty :: Applicative m => EmptyC m a -> m ()
evalEmpty = void . runEmpty

execEmpty :: Applicative m => EmptyC m a -> m Bool
execEmpty = fmap isJust . runEmpty

newtype EmptyC m a = EmptyC { runEmptyC :: forall r . (a -> m r) -> m r -> m r }
  deriving (Functor)

instance Applicative (EmptyC m) where
  pure a = EmptyC (\ just _ -> just a)
  (<*>) = ap

instance Monad (EmptyC m) where
  EmptyC m >>= f = EmptyC (\ just nothing -> m (\ a -> runEmptyC (f a) just nothing) nothing)

instance MonadFail m => MonadFail (EmptyC m) where
  fail = lift . fail

instance MonadIO m => MonadIO (EmptyC m) where
  liftIO = lift . liftIO

instance MonadTrans EmptyC where
  lift m = EmptyC (\ just _ -> m >>= just)
