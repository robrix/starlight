{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Unit
( Milli(..)
, getMilli
, milli
, unMilli
) where

import Data.Proxy
import Foreign.Storable
import GL.Type as GL
import GL.Uniform

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
