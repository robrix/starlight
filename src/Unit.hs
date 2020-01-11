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
, dim
, unDim
) where

import Control.Applicative (liftA2)
import Control.Lens ((^.))
import Control.Lens.Iso
import Data.Coerce
import Data.Functor.Const
import Data.Proxy
import Foreign.Storable
import GHC.Generics ((:.:)(..))
import GHC.TypeLits
import GL.Type as GL
import GL.Uniform
import Linear.Metric
import Linear.V4
import Linear.Vector
import Numeric

-- * Units

class Applicative u => Unit u where
  prj :: u a -> a
  default prj :: Coercible (u a) a => u a -> a
  prj = coerce

  factor :: Fractional a => Const a (u a)
  factor = 1

  suffix :: Const ShowS (u a)

unitary :: forall u a b . (Unit u, Fractional a, Fractional b) => Iso (u a) (u b) a b
unitary = iso ((* getConst (factor @u)) . prj) (pure . (/ getConst (factor @u)))

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
  prj = prj . getMult

  factor = fromIntegral (natVal (Proxy @n)) / fromIntegral (natVal (Proxy @d))

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
  prj = prj . prj . getPrd
  factor = Const (getConst (factor @u) * getConst (factor @v))
  suffix = Const (getConst (suffix @u) . ('·' :) . getConst (suffix @v))


newtype (u :/: v) a = Per { getPer :: u (v a) }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Applicative via u :.: v

infixl 7 :/:

instance (Applicative u, Additive v) => Additive (u :/: v) where
  zero = Per (pure zero)
  liftU2 f (Per a) (Per b) = Per (liftA2 (liftU2 f) a b)
  liftI2 f (Per a) (Per b) = Per (liftA2 (liftI2 f) a b)

instance (Applicative u, Foldable u, Additive v, Foldable v) => Metric (u :/: v)

instance (Unit u, Unit v) => Unit (u :/: v) where
  prj = prj . prj . getPer
  factor = Const (getConst (factor @v) / getConst (factor @u))
  suffix = Const (getConst (suffix @u) . ('/' :) . getConst (suffix @v))


newtype (u :^: (n :: Nat)) a = Exp { getExp :: u a }
  deriving (Additive, Applicative, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)

infixr 8 :^:

instance (KnownNat n, Unit u) => Unit (u :^: n) where
  prj = prj . getExp
  factor = Const (getConst (factor @u) ^ natVal (Proxy @n))
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

-- | NB: does not perform conversion!
instance (Unit u, R1 v) => R1 (u :# v) where
  _x = iso getDim Dim .iso prj pure._x

-- | NB: does not perform conversion!
instance (Unit u, R2 v) => R2 (u :# v) where
  _y = iso getDim Dim .iso prj pure._y
  _xy = iso getDim Dim .iso prj pure._xy

-- | NB: does not perform conversion!
instance (Unit u, R3 v) => R3 (u :# v) where
  _z = iso getDim Dim .iso prj pure._z
  _xyz = iso getDim Dim .iso prj pure._xyz

-- | NB: does not perform conversion!
instance (Unit u, R4 v) => R4 (u :# v) where
  _w = iso getDim Dim .iso prj pure._w
  _xyzw = iso getDim Dim .iso prj pure._xyzw

dimensional :: (Unit u, Fractional (v a), Fractional (v b)) => Iso ((u :# v) a) ((u :# v) b) (v a) (v b)
dimensional = iso getDim Dim .unitary

unDim :: (Unit u, Fractional (v a)) => (u :# v) a -> v a
unDim = (^.dimensional)

dim :: (Unit u, Fractional (v a)) => v a -> (u :# v) a
dim = (^.from dimensional)
