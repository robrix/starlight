{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Unit.Power
( Watts(..)
, getWatts
, module Unit
) where

import Foreign.Storable
import GL.Type as GL
import GL.Uniform
import Unit

newtype Watts a = Watts a
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)

getWatts :: Watts a -> a
getWatts (Watts a) = a
