{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Unit.Mass
( Grams(..)
, getGrams
, getKilograms
, module Unit
) where

import Data.Proxy
import Foreign.Storable
import GL.Type as GL
import GL.Uniform
import Unit

newtype Grams a = Grams a
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, Uniform)

instance GL.Type a => GL.Type (Grams a) where
  glType _ = glType (Proxy @a)

  glDims _ = glDims (Proxy @a)

getGrams :: Grams a -> a
getGrams (Grams a) = a

getKilograms :: Kilo Grams a -> a
getKilograms = getGrams . getKilo
