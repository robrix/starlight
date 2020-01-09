{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Unit
( Milli(..)
, getMilli
, milli
, unMilli
, Kilo(..)
, getKilo
, kilo
, unKilo
, Delta(..)
, Unit(..)
, Mult(..)
) where

import Data.Proxy
import Foreign.Storable
import GHC.TypeLits
import GL.Type as GL
import GL.Uniform
import Linear.Metric
import Linear.Vector

newtype Milli f a = Milli (f a)
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, Uniform)

instance GL.Type (f a) => GL.Type (Milli f a) where
  glType _ = glType (Proxy @(f a))

  glDims _ = glDims (Proxy @(f a))

getMilli :: Milli f a -> f a
getMilli (Milli fa) = fa

milli :: (Num a, Functor f) => f a -> Milli f a
milli fa = Milli (fa ^* 1000)

unMilli :: (Fractional a, Functor f) => Milli f a -> f a
unMilli (Milli fa) = fa ^/ 1000


newtype Kilo f a = Kilo (f a)
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, Uniform)

instance GL.Type (f a) => GL.Type (Kilo f a) where
  glType _ = glType (Proxy @(f a))

  glDims _ = glDims (Proxy @(f a))

getKilo :: Kilo f a -> f a
getKilo (Kilo fa) = fa

kilo :: (Fractional a, Functor f) => f a -> Kilo f a
kilo fa = Kilo (fa ^/ 1000)

unKilo :: (Num a, Functor f) => Kilo f a -> f a
unKilo (Kilo fa) = fa ^* 1000


newtype Delta f a = Delta { getDelta :: f a }
  deriving (Additive, Eq, Foldable, Floating, Fractional, Functor, Metric, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, Uniform)

instance GL.Type (f a) => GL.Type (Delta f a) where
  glType _ = glType (Proxy @(f a))

  glDims _ = glDims (Proxy @(f a))


class Functor u => Unit u where
  un :: Fractional a => u a -> a

instance Unit u => Unit (Kilo u) where
  un = un . unKilo

instance Unit u => Unit (Milli u) where
  un = un . unMilli


newtype Mult (n :: Nat) (d :: Nat) u a = Mult (u a)
 deriving (Additive, Eq, Foldable, Floating, Fractional, Functor, Metric, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable)

instance GL.Type (f a) => GL.Type (Mult n d f a) where
  glType _ = glType (Proxy @(f a))

  glDims _ = glDims (Proxy @(f a))
