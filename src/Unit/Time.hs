{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Unit.Time
( Time
, Seconds(..)
, fromDays
, fromHours
, fromMinutes
, Minutes(..)
, Hours(..)
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
  deriving (Column, Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

instance Unit Seconds where
  type Dim Seconds = Time
  suffix = K ('s':)

-- | Convert days to 'Seconds'. Note that this does not take e.g. leap seconds into account.
fromDays :: Floating a => a -> Seconds a
fromDays d = fromHours (Hours (d * 24))

fromHours :: Floating a => Hours a -> Seconds a
fromHours = convert

fromMinutes :: Floating a => Minutes a -> Seconds a
fromMinutes = convert


newtype Minutes a = Minutes { getMinutes :: a }
  deriving (Column, Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

instance Unit Minutes where
  type Dim Minutes = Time
  factor = K 60
  suffix = K ("min"++)


newtype Hours a = Hours { getHours :: a }
  deriving (Column, Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

instance Unit Hours where
  type Dim Hours = Time
  factor = K 3600
  suffix = K ("h"++)
