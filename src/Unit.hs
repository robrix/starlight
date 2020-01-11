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
( -- * Units
  Unit(..)
, unitary
, un
, nu
  -- * Prefixes
, Mult(..)
  -- ** Submultiples
, Pico(..)
, Nano(..)
, Micro(..)
, Milli(..)
  -- ** Multiples
, Kilo(..)
, Mega(..)
  -- ** Formatting
, formatWith
, format
, formatDec
, formatExp
  -- * Change
, Delta(..)
  -- * Combinators
, (:/:)(..)
, (:*:)(..)
, (:^:)(..)
, (:#)(..)
, dimensional
) where

import Control.Applicative (liftA2)
import Control.Lens ((^.))
import Control.Lens.Iso
import Data.Coerce
import Data.Functor.Const
import Data.Functor.Identity
import Data.Proxy
import Foreign.Storable
import GHC.Generics ((:.:)(..))
import GHC.TypeLits
import GL.Type as GL
import GL.Uniform
import Linear.Metric
import Linear.Vector
import Numeric

-- * Units

class Applicative u => Unit u where
  unitary' :: (Fractional a, Fractional b) => AnIso (u a) (u b) a b
  default unitary' :: (Coercible a (u a), Coercible b (u b)) => AnIso (u a) (u b) a b
  unitary' = coerced

  suffix :: Const ShowS (u a)

unitary :: (Unit u, Fractional a, Fractional b) => Iso (u a) (u b) a b
unitary = cloneIso unitary

un :: (Unit u, Fractional a) => u a -> a
un = (^.unitary)

nu :: (Unit u, Fractional a) => a -> u a
nu = (^.from unitary)


-- ** Formatting

formatWith :: Unit u => (Maybe Int -> u a -> ShowS) -> Maybe Int -> u a -> String
formatWith with n u = with n u (getConst (suffix `asTypeOf` (u <$ Const ('x':))) "")

format :: (Unit u, RealFloat (u a)) => Maybe Int -> u a -> String
format = formatWith showGFloat

formatDec :: (Unit u, RealFloat (u a)) => Maybe Int -> u a -> String
formatDec = formatWith showFFloat

formatExp :: (Unit u, RealFloat (u a)) => Maybe Int -> u a -> String
formatExp = formatWith showEFloat


-- * Prefixes

newtype Mult (n :: Nat) (d :: Nat) (s :: Symbol) u a = Mult { getMult :: u a }
 deriving (Additive, Applicative, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)

instance (KnownNat n, KnownNat d, KnownSymbol s, Unit u) => Unit (Mult n d s u) where
  unitary' = iso getMult Mult .iso from to.unitary where
    from = (^* (fromIntegral (natVal (Proxy @n)) / fromIntegral (natVal (Proxy @d))))
    to   = (^* (fromIntegral (natVal (Proxy @d)) / fromIntegral (natVal (Proxy @n))))

  suffix = Const ((symbolVal (Proxy @s) ++) . getConst (suffix @u))


-- ** Submultiples

newtype Pico u a = Pico { getPico :: u a }
  deriving (Additive, Applicative, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Unit via Mult 1 1_000_000_000_000 "p" u

newtype Nano u a = Nano { getNano :: u a }
  deriving (Additive, Applicative, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Unit via Mult 1 1_000_000_000 "n" u

newtype Micro u a = Micro { getMicro :: u a }
  deriving (Additive, Applicative, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Unit via Mult 1 1_000_000 "μ" u

newtype Milli u a = Milli { getMilli :: u a }
  deriving (Additive, Applicative, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Unit via Mult 1 1_000 "m" u


-- ** Multiples

newtype Kilo u a = Kilo { getKilo :: u a }
  deriving (Additive, Applicative, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Unit via Mult 1_000 1 "k" u

newtype Mega u a = Mega { getMega :: u a }
  deriving (Additive, Applicative, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Unit via Mult 1_000_000 1 "M" u


-- * Change

newtype Delta u a = Delta { getDelta :: u a }
  deriving (Additive, Applicative, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)


-- * Combinators

newtype (u :*: v) a = Prd { getPrd :: u (v a) }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Applicative via u :.: v

infixl 7 :*:

instance (Applicative u, Additive v) => Additive (u :*: v) where
  zero = Prd (pure zero)
  liftU2 f (Prd a) (Prd b) = Prd (liftA2 (liftU2 f) a b)
  liftI2 f (Prd a) (Prd b) = Prd (liftA2 (liftI2 f) a b)

instance (Applicative u, Foldable u, Additive v, Foldable v) => Metric (u :*: v)

instance (Unit u, Unit v) => Unit (u :*: v) where
  unitary' = iso getPrd Prd .iso (fmap (^.unitary)) (fmap (^.from unitary)) .unitary
  suffix = Const (getConst (suffix @u) . ('·' :) . getConst (suffix @v))


newtype ((u :: * -> *) :/: (v :: * -> *)) a = Per { getPer :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via Identity

infixl 7 :/:

instance (Unit u, Unit v) => Unit (u :/: v) where
  suffix = Const (getConst (suffix @u) . ('/' :) . getConst (suffix @v))


newtype (u :^: (n :: Nat)) a = Exp { getExp :: u a }
  deriving (Additive, Applicative, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)

infixr 8 :^:

instance (KnownNat n, Unit u) => Unit (u :^: n) where
  unitary' = iso getExp Exp .unitary
  suffix = Const (getConst (suffix @u) . (digits (fromIntegral (natVal (Proxy @n))) ++)) where
    digits n = go "" n where
      go s n | n >= 10   = let (q, r) = n `quotRem` 10 in go ((sup !! r):s) q
             | otherwise = (sup !! n):s
    sup = "⁰¹²³⁴⁵⁶⁷⁸⁹"


newtype (u :# v) a = Dim { getDim :: u (v a) }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Applicative via u :.: v

infixl 4 :#

instance (Applicative u, Additive v) => Additive (u :# v) where
  zero = Dim (pure zero)
  liftU2 f (Dim a) (Dim b) = Dim (liftA2 (liftU2 f) a b)
  liftI2 f (Dim a) (Dim b) = Dim (liftA2 (liftI2 f) a b)

instance (Applicative u, Foldable u, Additive v, Foldable v) => Metric (u :# v)

dimensional :: (Unit u, Fractional (v a), Fractional (v b)) => Iso ((u :# v) a) ((u :# v) b) (v a) (v b)
dimensional = iso getDim Dim .unitary
