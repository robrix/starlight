{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Unit.Count
( Count(..)
, module Unit
, module Unit.Algebra
, module Unit.Multiple
) where

import Data.Functor.I
import Data.Functor.K
import Data.Proxy
import Foreign.Storable
import GHC.TypeLits
import GL.Type as GL
import GL.Uniform
import Linear
import System.Random (Random)
import Unit
import Unit.Algebra
import Unit.Multiple

newtype Count (sym :: Symbol) a = Count { getCount :: a }
  deriving (Column, Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Random, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

instance Dimension (Count sym)
instance KnownSymbol sym => Pow (Count sym) (Count sym) (Count sym) n (Count sym)

instance KnownSymbol sym => Unit (Count sym) (Count sym) where
  suffix = K (symbolVal (Proxy @sym) ++)
