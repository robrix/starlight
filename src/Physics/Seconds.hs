{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Physics.Seconds
( Seconds(..)
) where

import GL.Uniform

newtype Seconds a = Seconds { getSeconds :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Traversable, Uniform)
