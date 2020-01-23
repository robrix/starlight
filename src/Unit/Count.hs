{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Unit.Count
( Count(..)
) where

import Data.Functor.I
import Foreign.Storable
import GL.Type as GL
import GL.Uniform
import Linear

newtype Count a = Count { getCount :: a }
  deriving (Column, Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I
