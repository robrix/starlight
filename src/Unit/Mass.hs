{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Unit.Mass
( Grams(..)
, getGrams
, getKilograms
, module Unit
) where

import Foreign.Storable
import GL.Type as GL
import GL.Uniform
import Unit

newtype Grams a = Grams a
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)

instance Unit Grams

getGrams :: Grams a -> a
getGrams (Grams a) = a

getKilograms :: Kilo Grams a -> a
getKilograms = getGrams . getKilo
