{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, UndecidableInstances #-}
module Control.Monad.IO.Class.Lift
( LiftIO(..)
, module Control.Carrier.Lift
, MonadIO(..)
) where

import Control.Algebra
import Control.Carrier.Lift
import Control.Monad.IO.Class

newtype LiftIO m a = LiftIO { runLiftIO :: m a }
  deriving (Applicative, Functor, Monad)

instance Has (Lift IO) sig m => MonadIO (LiftIO m) where
  liftIO = sendM

instance Algebra sig m => Algebra sig (LiftIO m) where
  alg = LiftIO . alg . handleCoercible
