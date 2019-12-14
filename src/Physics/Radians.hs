{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Physics.Radians
( Radians(..)
, cartesian2
) where

import GL.Uniform
import Linear.V2

newtype Radians a = Radians { getRadians :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Traversable, Uniform)

cartesian2 :: Floating a => a -> Radians a -> V2 a
cartesian2 r (Radians phi) = V2 (r * cos phi) (r * sin phi)
