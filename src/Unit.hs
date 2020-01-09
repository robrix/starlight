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
  -- * Exponents
, Square(..)
, Cubic(..)
  -- * Units
, Unit(..)
, unitary
  -- ** Formatting
, formatWith
, format
, formatDec
, formatExp
  -- * Change
, Delta(..)
, (:/:)(..)
, (:*:)(..)
, (:^:)(..)
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

-- * Prefixes

newtype Mult (n :: Nat) (d :: Nat) (s :: Symbol) u a = Mult { getMult :: u a }
 deriving (Additive, Eq, Foldable, Floating, Fractional, Functor, Metric, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)

instance (KnownNat n, KnownNat d, KnownSymbol s, Unit u) => Unit (Mult n d s u) where
  un = un   . (^* (fromIntegral (natVal (Proxy @n)) / fromIntegral (natVal (Proxy @d)))) . getMult
  nu = Mult . (^* (fromIntegral (natVal (Proxy @d)) / fromIntegral (natVal (Proxy @n)))) . nu

  suffix = Const ((symbolVal (Proxy @s) ++) . getConst (suffix @u))


-- ** Submultiples

newtype Pico u a = Pico { getPico :: u a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Unit via (Mult 1 1_000_000_000_000 "p" u)

newtype Nano u a = Nano { getNano :: u a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Unit via (Mult 1 1_000_000_000 "n" u)

newtype Micro u a = Micro { getMicro :: u a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Unit via (Mult 1 1_000_000 "μ" u)


-- ** Multiples

newtype Milli u a = Milli { getMilli :: u a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Unit via (Mult 1 1_000 "m" u)

newtype Kilo u a = Kilo { getKilo :: u a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Unit via (Mult 1_000 1 "k" u)

newtype Mega u a = Mega { getMega :: u a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Unit via (Mult 1_000_000 1 "M" u)


-- * Exponents

newtype Square u a = Square { getSquare :: u a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)

instance Unit u => Unit (Square u) where
  un = un . getSquare
  nu = Square . nu
  suffix = Const (getConst (suffix @u) . ('²':))

newtype Cubic u a = Cubic { getCubic :: u a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)

instance Unit u => Unit (Cubic u) where
  un = un . getCubic
  nu = Cubic . nu
  suffix = Const (getConst (suffix @u) . ('³':))


-- * Units

class Functor u => Unit u where
  un :: Fractional a => u a -> a
  default un :: Coercible (u a) a => u a -> a
  un = coerce
  nu :: Fractional a => a -> u a
  default nu :: Coercible a (u a) => a -> u a
  nu = coerce

  suffix :: Const ShowS (u a)

unitary :: (Fractional a, Fractional b, Unit u) => Iso (u a) (u b) a b
unitary = iso un nu


-- ** Formatting

formatWith :: Unit u => (Maybe Int -> u a -> ShowS) -> Maybe Int -> u a -> String
formatWith with n u = with n u (getConst (suffix `asTypeOf` (u <$ Const ('x':))) "")

format :: (Unit u, RealFloat (u a)) => Maybe Int -> u a -> String
format = formatWith showGFloat

formatDec :: (Unit u, RealFloat (u a)) => Maybe Int -> u a -> String
formatDec = formatWith showFFloat

formatExp :: (Unit u, RealFloat (u a)) => Maybe Int -> u a -> String
formatExp = formatWith showEFloat


-- * Change

newtype Delta u a = Delta { getDelta :: u a }
  deriving (Additive, Eq, Foldable, Floating, Fractional, Functor, Metric, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)

newtype ((u :: * -> *) :*: (v :: * -> *)) a = Prd { getPrd :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)

instance (Unit f, Unit g) => Unit (f :*: g) where
  suffix = Const (getConst (suffix @f) . ('·' :) . getConst (suffix @g))

newtype ((f :: * -> *) :/: (g :: * -> *)) a = Per { getPer :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)

instance (Unit f, Unit g) => Unit (f :/: g) where
  suffix = Const (getConst (suffix @f) . ('/' :) . getConst (suffix @g))

newtype (u :^: (n :: Nat)) a = Exp { getExp :: u a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)

instance (KnownNat n, Unit u) => Unit (u :^: n) where
  un = un . getExp
  nu = Exp . nu
  suffix = Const (getConst (suffix @u) . (digits (fromIntegral (natVal (Proxy @n))) ++)) where
    digits n = go "" n where
      go s n | n >= 10   = let (q, r) = n `quotRem` 10 in go ((sup !! r):s) q
             | otherwise = (sup !! n):s
    sup = "⁰¹²³⁴⁵⁶⁷⁸⁹"
