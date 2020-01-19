{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
module Unit.Vector
( (:#)(..)
) where

import Foreign.Storable
import GHC.Generics ((:.:)(..))
import GL.Type as GL
import GL.Uniform
import Linear.Conjugate
import Linear.Exts

newtype (v :# u) a = V { getV :: v (u a) }
  deriving (Column, Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Applicative via v :.: u

infixr 3 :#
