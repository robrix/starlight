{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Unit.Count
( Count(..)
) where

import Data.Functor.I
import Data.Functor.K
import Foreign.Storable
import GL.Type as GL
import GL.Uniform
import Linear
import Unit

newtype Count a = Count { getCount :: a }
  deriving (Column, Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

instance Unit Count where
  type Dim Count = Count
  suffix = K (""++)
