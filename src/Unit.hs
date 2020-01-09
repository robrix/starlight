{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Unit
( Milli(..)
, Kilo(..)
, Mega(..)
, Delta(..)
, Unit(..)
, unitary
, Mult(..)
) where

import Control.Lens.Iso
import Data.Coerce
import Data.Proxy
import Foreign.Storable
import GHC.TypeLits
import GL.Type as GL
import GL.Uniform
import Linear.Metric
import Linear.Vector

newtype Milli u a = Milli { getMilli :: u a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Unit via (Mult 1 1_000 u)

newtype Kilo u a = Kilo { getKilo :: u a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Unit via (Mult 1_000 1 u)

newtype Mega u a = Mega { getMega :: u a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Unit via (Kilo (Kilo u))

newtype Delta u a = Delta { getDelta :: u a }
  deriving (Additive, Eq, Foldable, Floating, Fractional, Functor, Metric, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)


class Functor u => Unit u where
  un :: Fractional a => u a -> a
  default un :: Coercible (u a) a => u a -> a
  un = coerce
  nu :: Fractional a => a -> u a
  default nu :: Coercible a (u a) => a -> u a
  nu = coerce

unitary :: (Fractional a, Fractional b, Unit u) => Iso (u a) (u b) a b
unitary = iso un nu


newtype Mult (n :: Nat) (d :: Nat) u a = Mult { getMult :: u a }
 deriving (Additive, Eq, Foldable, Floating, Fractional, Functor, Metric, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)

instance (KnownNat n, KnownNat d, Unit u) => Unit (Mult n d u) where
  un = un . (^* (fromIntegral (natVal (Proxy @n)) / fromIntegral (natVal (Proxy @d)))) . getMult
  nu = Mult . (^* (fromIntegral (natVal (Proxy @d)) / fromIntegral (natVal (Proxy @n)))) . nu
