{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Unit
( Milli(..)
) where

import Data.Proxy
import Foreign.Storable
import GL.Type as GL
import GL.Uniform

newtype Milli f a = Milli { getMilli :: f a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, Uniform)

instance GL.Type (f a) => GL.Type (Milli f a) where
  glType _ = glType (Proxy @(f a))

  glDims _ = glDims (Proxy @(f a))
