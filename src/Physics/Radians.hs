{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Physics.Radians
( Radians(..)
) where

import GL.Uniform

newtype Radians a = Radians { getRadians :: a }
  deriving (Eq, Floating, Fractional, Num, Ord, Real, RealFloat, RealFrac, Show, Uniform)
