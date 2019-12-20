{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Unit.Time
( Seconds(..)
, Days(..)
) where

import GL.Uniform

newtype Seconds a = Seconds { getSeconds :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Traversable, Uniform)

newtype Days a = Days { getDays :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Traversable, Uniform)
