{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Unit.Angle
( Radians(..)
, fromDegrees
, cartesian2
, polar2
, Degrees(..)
) where

import GL.Uniform
import Linear.V2

newtype Radians a = Radians { getRadians :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Traversable, Uniform)

fromDegrees :: Floating a => Degrees a -> Radians a
fromDegrees (Degrees d) = Radians (d * pi / 180)


cartesian2 :: Floating a => Radians a -> a -> V2 a
cartesian2 (Radians phi) r = V2 (r * cos phi) (r * sin phi)

polar2 :: RealFloat a => V2 a -> (Radians a, a)
polar2 (V2 x y) = (Radians phi, r) where
  phi = atan2 y x
  r = sqrt (x * x + y * y)


newtype Degrees a = Degrees { getDegrees :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Traversable, Uniform)
