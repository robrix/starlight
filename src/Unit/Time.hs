{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Unit.Time
( Time
, Seconds(..)
, fromDays
, fromHours
, fromMinutes
, Minutes
, Hours
, module Unit
, module Unit.Multiple
) where

import Data.Functor.I
import Data.Functor.K
import Foreign.Storable
import GL.Type as GL
import GL.Uniform
import Linear.Conjugate
import Linear.Epsilon
import Linear.Metric
import Linear.Vector
import Unit
import Unit.Multiple

data Time a

newtype Seconds a = Seconds { getSeconds :: a }
  deriving (Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

instance Unit Time Seconds where
  suffix = K ('s':)

-- | Convert days to 'Seconds'. Note that this does not take e.g. leap seconds into account.
fromDays :: Num a => a -> Seconds a
fromDays d = fromHours (d * 24)

fromHours :: Num a => a -> Seconds a
fromHours h = fromMinutes (h * 60)

fromMinutes :: Num a => a -> Seconds a
fromMinutes m = Seconds (m * 60)

type Minutes = Mult 60 1 "min" Seconds

type Hours = Mult 3600 1 "h" Seconds
