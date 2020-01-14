{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector
import Unit
import Unit.Multiple

data Length a

newtype Metres a = Metres { getMetres :: a }
  deriving (Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via Identity

instance Unit Length Metres where suffix = Const ('m':)

deriving via V2 Float instance Uniform (V2 (Kilo Metres Float))
deriving via V3 Float instance Uniform (V3 (Kilo Metres Float))
deriving via V4 Float instance Uniform (V4 (Kilo Metres Float))

deriving via V2 Float instance Uniform (V2 (Mega Metres Float))
deriving via V3 Float instance Uniform (V3 (Mega Metres Float))
deriving via V4 Float instance Uniform (V4 (Mega Metres Float))

fromAUs :: Num a => a -> Metres a
fromAUs a = Metres (149597870700 * a)
