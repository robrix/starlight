{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.Time
( -- * Time carrier
  TimeC(..)
  -- * Time effect
, module Control.Effect.Time
) where

import Control.Effect.Time
import Control.Monad.IO.Class

newtype TimeC m a = TimeC (m a)
  deriving (Applicative, Functor, Monad, MonadIO)
