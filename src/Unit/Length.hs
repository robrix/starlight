{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Unit.Length
( Metres(..)
, getMetres
, fromKilometres
, fromAUs
, Kilometres(..)
, module Unit
) where

import Data.Proxy
import Foreign.Storable
import GL.Type as GL
import GL.Uniform
import Unit

newtype Metres a = Metres a
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, Uniform)

instance GL.Type a => GL.Type (Metres a) where
  glType _ = glType (Proxy @a)

  glDims _ = glDims (Proxy @a)

getMetres :: Metres a -> a
getMetres (Metres a) = a

fromKilometres :: Num a => Kilometres a -> Metres a
fromKilometres (Kilometres k) = Metres (k * 1000)

fromAUs :: Num a => a -> Metres a
fromAUs a = Metres (149597870700 * a)


newtype Kilometres a = Kilometres { getKilometres :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, Uniform)

instance GL.Type a => GL.Type (Kilometres a) where
  glType _ = glType (Proxy @a)

  glDims _ = glDims (Proxy @a)
