{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Unit.Length
( Kilometres(..)
) where

import GL.Uniform

newtype Kilometres a = Kilometres { getKilometres :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Traversable, Uniform)
