{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Unit.Mass
( Kilograms(..)
, module Unit
) where

import Data.Proxy
import Foreign.Storable
import GL.Type as GL
import GL.Uniform
import Unit

newtype Kilograms a = Kilograms { getKilograms :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, Uniform)

instance GL.Type a => GL.Type (Kilograms a) where
  glType _ = glType (Proxy @a)

  glDims _ = glDims (Proxy @a)
