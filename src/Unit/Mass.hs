{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Unit.Mass
( Grams(..)
, module Unit
) where

import Data.Functor.Const
import Foreign.Storable
import GL.Type as GL
import GL.Uniform
import Unit

newtype Grams a = Grams { getGrams :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)

instance Unit Grams where suffix = Const ('g':)
