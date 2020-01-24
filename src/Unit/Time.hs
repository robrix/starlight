{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Unit.Time
( Time
, Seconds(..)
, Minutes(..)
, Hours(..)
, Days(..)
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

data Time a

instance Dimension Time
instance Unit Time u => Pow Time u n (u :^: n)


newtype Seconds a = Seconds { getSeconds :: a }
  deriving (Column, Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Random, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

instance Unit Time Seconds where
  suffix = K ('s':)


newtype Minutes a = Minutes { getMinutes :: a }
  deriving (Column, Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Random, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

instance Unit Time Minutes where
  factor = K 60
  suffix = K ("min"++)


newtype Hours a = Hours { getHours :: a }
  deriving (Column, Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Random, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

instance Unit Time Hours where
  factor = K 3600
  suffix = K ("h"++)


newtype Days a = Days { getDays :: a }
  deriving (Column, Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Random, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

-- | Note that this does not take e.g. leap seconds into account.
instance Unit Time Days where
  factor = K 86400
  suffix = K ("d"++)
