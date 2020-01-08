{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Unit.Power
( Watts(..)
, getWatts
, module Unit
) where

import Data.Proxy
import Foreign.Storable
import GL.Type as GL
import GL.Uniform
import Unit

newtype Watts a = Watts a
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, Uniform)

instance GL.Type a => GL.Type (Watts a) where
  glType _ = glType (Proxy @a)

  glDims _ = glDims (Proxy @a)

getWatts :: Watts a -> a
getWatts (Watts a) = a
