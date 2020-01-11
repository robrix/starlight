{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Unit.Multiple
( -- * Prefixes
  Mult(..)
  -- ** Submultiples
, Pico(..)
, Nano(..)
, Micro(..)
, Milli(..)
  -- ** Multiples
, Kilo(..)
, Mega(..)
) where

import Data.Functor.Const
import Data.Proxy
import Foreign.Storable
import GHC.TypeLits
import GL.Type as GL
import GL.Uniform
import Linear.Conjugate
import Linear.Epsilon
import Linear.Metric
import Linear.Vector
import Unit

-- * Prefixes

newtype Mult (n :: Nat) (d :: Nat) (s :: Symbol) u a = Mult { getMult :: u a }
 deriving (Additive, Applicative, Conjugate, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)

instance (KnownNat n, KnownNat d, KnownSymbol s, Unit u) => Unit (Mult n d s u) where
  prj = prj . getMult

  factor = fromIntegral (natVal (Proxy @n)) / fromIntegral (natVal (Proxy @d))

  suffix = Const ((symbolVal (Proxy @s) ++) . getConst (suffix @u))


-- ** Submultiples

newtype Pico u a = Pico { getPico :: u a }
  deriving (Additive, Applicative, Conjugate, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Unit via Mult 1 1_000_000_000_000 "p" u

newtype Nano u a = Nano { getNano :: u a }
  deriving (Additive, Applicative, Conjugate, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Unit via Mult 1 1_000_000_000 "n" u

newtype Micro u a = Micro { getMicro :: u a }
  deriving (Additive, Applicative, Conjugate, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Unit via Mult 1 1_000_000 "μ" u

newtype Milli u a = Milli { getMilli :: u a }
  deriving (Additive, Applicative, Conjugate, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Unit via Mult 1 1_000 "m" u


-- ** Multiples

newtype Kilo u a = Kilo { getKilo :: u a }
  deriving (Additive, Applicative, Conjugate, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Unit via Mult 1_000 1 "k" u

newtype Mega u a = Mega { getMega :: u a }
  deriving (Additive, Applicative, Conjugate, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Unit via Mult 1_000_000 1 "M" u