{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, UndecidableInstances #-}
module Control.Monad.IO.Class.Lift
( Lifting(..)
, module Control.Carrier.Lift
, MonadIO(..)
) where

import Control.Algebra
import Control.Carrier.Lift
import Control.Monad.IO.Class

newtype Lifting m a = Lifting { runLifting :: m a }
  deriving (Applicative, Functor, Monad)

instance Has (Lift IO) sig m => MonadIO (Lifting m) where
  liftIO = sendM

instance Algebra sig m => Algebra sig (Lifting m) where
  alg = Lifting . alg . handleCoercible
