{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Unit.Length
( Metres(..)
, getMetres
, fromAUs
, Len(..)
, getLen
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

fromAUs :: Num a => a -> Metres a
fromAUs a = Metres (149597870700 * a)


newtype Len u a = Len (u a)

getLen :: Len u a -> u a
getLen (Len u) = u
