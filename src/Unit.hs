{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Unit
( -- * Units
  Unit(..)
, unitary
, un
, nu
  -- ** Formatting
, formatWith
, format
, formatDec
, formatExp
  -- * Change
, Delta(..)
) where

import Control.Lens ((^.))
import Control.Lens.Iso
import Data.Coerce
import Data.Functor.Const
import Foreign.Storable
import GL.Type as GL
import GL.Uniform
import Linear.Conjugate
import Linear.Epsilon
import Linear.Metric
import Linear.Vector
import Numeric

-- * Units

class Applicative u => Unit u where
  prj :: u a -> a
  default prj :: Coercible (u a) a => u a -> a
  prj = coerce

  factor :: Fractional a => Const a (u a)
  factor = 1

  suffix :: Const ShowS (u a)

unitary :: forall u a b . (Unit u, Fractional a, Fractional b) => Iso (u a) (u b) a b
unitary = iso ((* getConst (factor @u)) . prj) (pure . (/ getConst (factor @u)))

un :: (Unit u, Fractional a) => u a -> a
un = (^.unitary)

nu :: (Unit u, Fractional a) => a -> u a
nu = (^.from unitary)


-- ** Formatting

formatWith :: Unit u => (Maybe Int -> u a -> ShowS) -> Maybe Int -> u a -> String
formatWith with n u = with n u (getConst (suffix `asTypeOf` (u <$ Const ('x':))) "")

format :: (Unit u, RealFloat (u a)) => Maybe Int -> u a -> String
format = formatWith showGFloat

formatDec :: (Unit u, RealFloat (u a)) => Maybe Int -> u a -> String
formatDec = formatWith showFFloat

formatExp :: (Unit u, RealFloat (u a)) => Maybe Int -> u a -> String
formatExp = formatWith showEFloat


-- * Change

newtype Delta u a = Delta { getDelta :: u a }
  deriving (Additive, Applicative, Conjugate, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
