{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Unit.Length
( Metres(..)
, fromAUs
, module Unit
) where

import Data.Functor.Const
import Foreign.Storable
import GL.Type as GL
import GL.Uniform
import Unit

newtype Metres a = Metres { getMetres :: a }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)

instance Unit Metres where suffix = Const ('m':)

fromAUs :: Num a => a -> Metres a
fromAUs a = Metres (149597870700 * a)
