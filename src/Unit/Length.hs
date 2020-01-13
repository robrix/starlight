{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Unit.Length
( Length
, Metres(..)
, fromAUs
, module Unit
, module Unit.Multiple
) where

import Data.Functor.Const
import Data.Functor.Identity
import Foreign.Storable
import GL.Type as GL
import GL.Uniform
import Linear.Conjugate
import Linear.Epsilon
import Linear.Metric
import Linear.Vector
import Unit
import Unit.Multiple

data Length a

newtype Metres a = Metres { getMetres :: a }
  deriving (Conjugate, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via Identity

instance Unit Length Metres where suffix = Const ('m':)

fromAUs :: Num a => a -> Metres a
fromAUs a = Metres (149597870700 * a)
