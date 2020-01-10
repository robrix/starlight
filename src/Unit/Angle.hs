{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Unit.Angle
( Radians(..)
, fromDegrees
, Degrees(..)
, module Unit
) where

import Data.Functor.Const
import Data.Functor.Identity
import Foreign.Storable
import GL.Type as GL
import GL.Uniform
import Linear.Metric
import Linear.Vector
import Unit

newtype Radians a = Radians { getRadians :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via Identity

instance Unit Radians where suffix = Const ("rad"++)

fromDegrees :: Floating a => Degrees a -> Radians a
fromDegrees (Degrees d) = Radians (d * pi / 180)


newtype Degrees a = Degrees { getDegrees :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via Identity

instance Unit Degrees where suffix = Const ('°':)
