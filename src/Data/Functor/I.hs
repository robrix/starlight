{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | I got sick of writing 'Identity' out in full.
module Data.Functor.I
( I(..)
) where

import Data.Functor.Identity
import Linear
import Foreign.Storable
import GL.Type as GL
import GL.Uniform

newtype I a = I { getI :: a }
  deriving (Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via Identity
