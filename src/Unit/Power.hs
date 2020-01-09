{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Unit.Power
( Watts(..)
, module Unit
) where

import Foreign.Storable
import GL.Type as GL
import GL.Uniform
import Unit

newtype Watts a = Watts { getWatts :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)

instance Unit Watts
