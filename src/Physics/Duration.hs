{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Physics.Duration
( Duration(..)
) where

import GL.Uniform

newtype Duration a = Duration { getDuration :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Traversable, Uniform)
