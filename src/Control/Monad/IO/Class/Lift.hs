{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.IO.Class.Lift
( LiftIO(..)
, module Control.Carrier.Lift
, MonadIO(..)
) where

import Control.Algebra
import Control.Carrier.Lift
import Control.Monad.IO.Class

newtype LiftIO m a = LiftIO { runLiftIO :: m a }
  deriving (Algebra sig, Applicative, Functor, Monad, MonadFail)

instance Has (Lift IO) sig m => MonadIO (LiftIO m) where
  liftIO = sendM
