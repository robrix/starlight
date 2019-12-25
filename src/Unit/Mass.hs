{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Unit.Mass
( Kilograms(..)
) where

import Foreign.Storable
import GL.Uniform

newtype Kilograms a = Kilograms { getKilograms :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, Uniform)
