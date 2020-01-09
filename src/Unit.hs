{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Unit
( Milli(..)
, Kilo(..)
, Mega(..)
, Delta(..)
, Unit(..)
, unitary
, formatWith
, format
, formatDec
, formatExp
, Mult(..)
, (:/:)(..)
) where

import Control.Lens.Iso
import Data.Coerce
import Data.Functor.Const
import Data.Proxy
import Foreign.Storable
import GHC.TypeLits
import GL.Type as GL
import GL.Uniform
import Linear.Metric
import Linear.Vector
import Numeric

newtype Micro u a = Micro { getMicro :: u a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Unit via (Mult 1 1_000_000 "μ" u)

newtype Milli u a = Milli { getMilli :: u a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Unit via (Mult 1 1_000 "m" u)

newtype Kilo u a = Kilo { getKilo :: u a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Unit via (Mult 1_000 1 "k" u)

newtype Mega u a = Mega { getMega :: u a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Unit via (Mult 1_000_000 1 "M" u)

newtype Delta u a = Delta { getDelta :: u a }
  deriving (Additive, Eq, Foldable, Floating, Fractional, Functor, Metric, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)


class Functor u => Unit u where
  un :: Fractional a => u a -> a
  default un :: Coercible (u a) a => u a -> a
  un = coerce
  nu :: Fractional a => a -> u a
  default nu :: Coercible a (u a) => a -> u a
  nu = coerce

  suffix :: Const String (u a)

unitary :: (Fractional a, Fractional b, Unit u) => Iso (u a) (u b) a b
unitary = iso un nu

formatWith :: Unit u => (Maybe Int -> u a -> ShowS) -> Maybe Int -> u a -> String
formatWith with n u = with n u (getConst (suffix `asTypeOf` (u <$ Const "")))

format :: (Unit u, RealFloat (u a)) => Maybe Int -> u a -> String
format = formatWith showGFloat

formatDec :: (Unit u, RealFloat (u a)) => Maybe Int -> u a -> String
formatDec = formatWith showFFloat

formatExp :: (Unit u, RealFloat (u a)) => Maybe Int -> u a -> String
formatExp = formatWith showEFloat


newtype Mult (n :: Nat) (d :: Nat) (s :: Symbol) u a = Mult { getMult :: u a }
 deriving (Additive, Eq, Foldable, Floating, Fractional, Functor, Metric, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)

instance (KnownNat n, KnownNat d, KnownSymbol s, Unit u) => Unit (Mult n d s u) where
  un = un   . (^* (fromIntegral (natVal (Proxy @n)) / fromIntegral (natVal (Proxy @d)))) . getMult
  nu = Mult . (^* (fromIntegral (natVal (Proxy @d)) / fromIntegral (natVal (Proxy @n)))) . nu

  suffix = Const (symbolVal (Proxy @s) ++ getConst (suffix @u))


newtype ((f :: * -> *) :/: (g :: * -> *)) a = Per { getPer :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
