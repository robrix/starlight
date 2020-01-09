{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Unit
( Milli(..)
, getMilli
, Kilo(..)
, getKilo
, kilo
, unKilo
, Delta(..)
, Unit(..)
, Mult(..)
, getMult
) where

import Data.Coerce
import Data.Proxy
import Foreign.Storable
import GHC.TypeLits
import GL.Type as GL
import GL.Uniform
import Linear.Metric
import Linear.Vector

newtype Milli f a = Milli (f a)
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Unit via (Mult 1 1000 f)

getMilli :: Milli f a -> f a
getMilli (Milli fa) = fa


newtype Kilo f a = Kilo (f a)
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Unit via (Mult 1000 1 f)

getKilo :: Kilo f a -> f a
getKilo (Kilo fa) = fa

kilo :: (Fractional a, Functor f) => f a -> Kilo f a
kilo fa = Kilo (fa ^/ 1000)

unKilo :: (Num a, Functor f) => Kilo f a -> f a
unKilo (Kilo fa) = fa ^* 1000


newtype Delta f a = Delta { getDelta :: f a }
  deriving (Additive, Eq, Foldable, Floating, Fractional, Functor, Metric, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)


class Functor u => Unit u where
  un :: Fractional a => u a -> a
  default un :: Coercible (u a) a => u a -> a
  un = coerce
  nu :: Fractional a => a -> u a
  default nu :: Coercible a (u a) => a -> u a
  nu = coerce


newtype Mult (n :: Nat) (d :: Nat) u a = Mult (u a)
 deriving (Additive, Eq, Foldable, Floating, Fractional, Functor, Metric, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)

instance (KnownNat n, KnownNat d, Unit u) => Unit (Mult n d u) where
  un = un . (^* (fromIntegral (natVal (Proxy @n)) / fromIntegral (natVal (Proxy @d)))) . getMult
  nu = Mult . (^* (fromIntegral (natVal (Proxy @d)) / fromIntegral (natVal (Proxy @n)))) . nu

getMult :: Mult n d u a -> u a
getMult (Mult u) = u
