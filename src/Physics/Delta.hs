{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Physics.Delta
( Delta(..)
) where

import Data.Proxy
import Foreign.Storable
import GL.Type as GL
import GL.Uniform
import Linear.Metric
import Linear.Vector

newtype Delta f a = Delta { getDelta :: f a }
  deriving (Additive, Eq, Foldable, Floating, Fractional, Functor, Metric, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, Uniform)

instance GL.Type (f a) => GL.Type (Delta f a) where
  glType _ = glType (Proxy @(f a))

  glDims _ = glDims (Proxy @(f a))
