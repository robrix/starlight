{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Time
( -- * Time carrier
  runTime
, TimeC(TimeC)
  -- * Time effect
, module Control.Effect.Time
) where

import Control.Algebra
import Control.Carrier.Lift
import Control.Effect.Time
import Control.Monad.IO.Class
import Data.Time.Clock

newtype TimeC m a = TimeC { runTime :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance Has (Lift IO) sig m => Algebra (Time :+: sig) (TimeC m) where
  alg = \case
    L (Now k) -> sendM getCurrentTime >>= k
    R other   -> TimeC (send (handleCoercible other))
