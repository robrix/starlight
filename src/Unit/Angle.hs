{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Unit.Angle
( Radians(..)
, fromDegrees
, Degrees(..)
) where

import Data.Proxy
import Foreign.Storable
import GL.Type as GL
import GL.Uniform

newtype Radians a = Radians { getRadians :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, Uniform)

instance GL.Type a => GL.Type (Radians a) where
  glType _ = glType (Proxy @a)

  glDims _ = glDims (Proxy @a)

fromDegrees :: Floating a => Degrees a -> Radians a
fromDegrees (Degrees d) = Radians (d * pi / 180)


newtype Degrees a = Degrees { getDegrees :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, Uniform)

instance GL.Type a => GL.Type (Degrees a) where
  glType _ = glType (Proxy @a)

  glDims _ = glDims (Proxy @a)
