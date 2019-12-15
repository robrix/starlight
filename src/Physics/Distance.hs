{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Physics.Distance
( Distance(..)
) where

import GL.Uniform

newtype Distance a = Distance { getDistance :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Traversable, Uniform)
