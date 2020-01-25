{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Unit.Mass
( Mass
, Grams(..)
, module Unit
, module Unit.Algebra
, module Unit.Multiple
) where

import Data.Functor.I
import Data.Functor.K
import Foreign.Storable
import GHC.TypeLits
import GL.Type as GL
import GL.Uniform
import Linear.Conjugate
import Linear.Epsilon
import Linear.Metric
import Linear.Vector
import System.Random (Random)
import Unit
import Unit.Algebra
import Unit.Multiple

data Mass a

instance Dimension Mass
instance (Unit Mass u, KnownNat n, n' ~ FromNat n) => Pow Mass (Mass :^: n) u n' (u :^: n)


newtype Grams a = Grams { getGrams :: a }
  deriving (Column, Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Random, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

instance Unit Mass Grams where
  suffix = K ('g':)
