{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.Profile.Time
( -- * Profile carrier
  ProfileC(..)
  -- * Profile effect
, module Control.Effect.Profile
) where

import Control.Effect.Profile
import Control.Monad.IO.Class

newtype ProfileC m a = ProfileC (m a)
  deriving (Applicative, Functor, Monad, MonadIO)
