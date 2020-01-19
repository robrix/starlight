{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | I got sick of writing 'Identity' out in full.
--
-- This functor is commonly used to:
--
-- * Represent the coordinates in 1-dimensional @"Data.Functor.Interval".'Data.Functor.Interval.Interval'@ used for e.g. vertex ranges.
-- * Hold vertices’ values when copying into an array buffer.
-- * Represent dimensionless units such as transcendental numbers, the arguments & results of trigonometric functions, angles, and ratios.
-- * Represent the traditional dimension 1 of dimensionless units.
module Data.Functor.I
( I(..)
) where

import Data.Functor.Identity
import Linear
import Foreign.Storable
import GL.Type as GL
import GL.Uniform

newtype I a = I { getI :: a }
  deriving (Column, Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via Identity
