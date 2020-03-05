{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Empty.CPS
( -- * Empty carrier
  runEmpty
, evalEmpty
, execEmpty
, EmptyC(EmptyC)
  -- * Empty effect
, module Control.Effect.Empty
) where

import Control.Algebra
import Control.Effect.Empty
import Control.Monad (ap, void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Maybe (isJust)

runEmpty :: Applicative m => EmptyC m a -> m (Maybe a)
runEmpty (EmptyC run) = run (pure . Just)
{-# INLINABLE runEmpty #-}

evalEmpty :: Applicative m => EmptyC m a -> m ()
evalEmpty = void . runEmpty
{-# INLINABLE evalEmpty #-}

execEmpty :: Applicative m => EmptyC m a -> m Bool
execEmpty = fmap isJust . runEmpty
{-# INLINABLE execEmpty #-}

newtype EmptyC m a = EmptyC { runEmptyC :: forall r . (a -> m (Maybe r)) -> m (Maybe r) }
  deriving (Functor)

instance Applicative (EmptyC m) where
  pure a = EmptyC ($ a)
  {-# INLINABLE pure #-}
  (<*>) = ap
  {-# INLINABLE (<*>) #-}

instance Monad (EmptyC m) where
  EmptyC m >>= f = EmptyC (\ k -> m (($ k) . runEmptyC . f))
  {-# INLINABLE (>>=) #-}

instance MonadFail m => MonadFail (EmptyC m) where
  fail = lift . fail
  {-# INLINABLE fail #-}

instance MonadIO m => MonadIO (EmptyC m) where
  liftIO = lift . liftIO
  {-# INLINABLE liftIO #-}

instance MonadTrans EmptyC where
  lift m = EmptyC (m >>=)
  {-# INLINABLE lift #-}

instance Algebra sig m => Algebra (Empty :+: sig) (EmptyC m) where
  alg ctx hdl = \case
    L Empty -> EmptyC (const (pure Nothing))
    R other -> EmptyC (\ k -> thread (Just ctx) (maybe (pure Nothing) (runEmpty . hdl)) other >>= maybe (pure Nothing) k)
