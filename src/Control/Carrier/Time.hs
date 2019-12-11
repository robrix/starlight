{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.Time
( -- * Time carrier
  runTime
, TimeC(TimeC)
  -- * Time effect
, module Control.Effect.Time
) where

import Control.Effect.Time
import Control.Monad.IO.Class

newtype TimeC m a = TimeC { runTime :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)
