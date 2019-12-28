{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Control.Effect.Time
( -- * Time effect
  Time(..)
, now
, since
, time
  -- * Re-exports
, Algebra
, Has
, run
) where

import Control.Algebra
import Data.Time.Clock
import GHC.Generics (Generic1)

data Time m k
  = Now (UTCTime -> m k)
  deriving (Functor, Generic1)

instance HFunctor Time
instance Effect Time


now :: Has Time sig m => m UTCTime
now = send (Now pure)

since :: Has Time sig m => UTCTime -> m NominalDiffTime
since t = send (Now (pure . flip diffUTCTime t))

time :: Has Time sig m => m a -> m (NominalDiffTime, a)
time m = do
  start <- now
  a <- m
  flip (,) a <$> since start
