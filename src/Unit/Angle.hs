{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Unit.Angle
( Radians(..)
, fromDegrees
, Degrees(..)
) where

import GL.Uniform

newtype Radians a = Radians { getRadians :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Traversable, Uniform)

fromDegrees :: Floating a => Degrees a -> Radians a
fromDegrees (Degrees d) = Radians (d * pi / 180)


newtype Degrees a = Degrees { getDegrees :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Traversable, Uniform)
