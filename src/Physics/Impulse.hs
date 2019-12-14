{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Physics.Impulse
( Impulse(..)
) where

import GL.Uniform

newtype Impulse a = Impulse { getImpulse :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Traversable, Uniform)
