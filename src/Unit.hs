{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
) where

import Data.Proxy
import Foreign.Storable
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

milli :: Num (f a) => f a -> Milli f a
milli fa = Milli (fa * 1000)

unMilli :: Fractional (f a) => Milli f a -> f a
unMilli (Milli fa) = fa / 1000


newtype Kilo f a = Kilo (f a)
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, Uniform)

instance GL.Type (f a) => GL.Type (Kilo f a) where
  glType _ = glType (Proxy @(f a))

  glDims _ = glDims (Proxy @(f a))

getKilo :: Kilo f a -> f a
getKilo (Kilo fa) = fa

kilo :: Fractional (f a) => f a -> Kilo f a
kilo fa = Kilo (fa / 1000)

unKilo :: Num (f a) => Kilo f a -> f a
unKilo (Kilo fa) = fa * 1000


newtype Delta f a = Delta { getDelta :: f a }
  deriving (Additive, Eq, Foldable, Floating, Fractional, Functor, Metric, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, Uniform)

instance GL.Type (f a) => GL.Type (Delta f a) where
  glType _ = glType (Proxy @(f a))

  glDims _ = glDims (Proxy @(f a))


class Unit u where
  un :: Fractional a => u a -> a
