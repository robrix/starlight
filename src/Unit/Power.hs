{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Unit.Power
( Power
, Watts(..)
, module Unit
, module Unit.Algebra
, module Unit.Multiple
) where

import Data.Functor.I
import Data.Functor.K
import Foreign.Storable
import GL.Type as GL
import GL.Uniform
import Linear.Conjugate
import Linear.Epsilon
import Linear.Metric
import Linear.Vector
import System.Random (Random)
import Unit
import Unit.Algebra
import Unit.Multiple

data Power a

instance Dimension Power
instance Unit Power u => Pow Power u n (u :^: n)


newtype Watts a = Watts { getWatts :: a }
  deriving (Column, Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Random, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

instance Unit Power Watts where
  suffix = K ('W':)
