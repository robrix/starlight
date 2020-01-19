{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Unit.Algebra
(  -- * Algebra
  (.*.)
, (./.)
  -- * Calculation
, qdU
  -- * Combinators
, (:*:)(..)
, (:/:)(..)
, (:^:)
, I(..)
) where

import Data.Functor.I
import Data.Functor.K
import Foreign.Storable
import GHC.TypeLits hiding (Div)
import GL.Type as GL
import GL.Uniform
import Linear.Conjugate
import Linear.Epsilon
import Linear.Metric
import Linear.Vector
import Unit

-- * Algebra

(.*.) :: (Unit du u, Unit dv v, Unit dw (Mul u v), Fractional a) => u a -> v a -> Mul u v a
u .*. v = pure (prj v * prj u)

infixl 7 .*.

(./.) :: (Unit du u, Unit dv v, Unit dw (Div u v), Fractional a) => u a -> v a -> Div u v a
u ./. v = pure (prj u / prj v)

infixl 7 ./.

type family Mul u v where
  Mul  u         I        = u               -- u * 1       = u
  Mul  I         v        = v               -- 1 * v       = v
  Mul (u :/: v)  v        = u               -- u / v * v   = u
  Mul  u        (v :*: w) = Mul (Mul u w) v -- u * (v * w) = (u * w) * v
  Mul  u        (v :/: w) = Div (Mul u v) w -- u * (v / w) = (u * v) / w
  Mul (u :*: v)  w        = Mul u w :*: v   -- (u * v) * w = (u * w) * v
  Mul (u :/: v)  w        = Mul u w :/: v   -- (u / v) * w = (u * w) / v
  Mul  u         v        = u :*: v         -- u * v       = u * v

type family Div u v where
  Div  u         I        = u               -- u / 1       = u
  Div (u :*: v)  v        = u               -- u * v / v   = u
  Div (u :*: v)  u        = v               -- u * v / u   = v
  Div  u        (v :*: w) = Div (Div u w) v -- u / (v * w) = (u / w) / v
  Div (u :*: v)  w        = Mul (Div u w) v -- (u * v) / w = (u / w) * v
  Div (u :/: v)  w        = Div u w :/: v   -- (u / v) / w = (u / w) / v
  Div  u         u        = I               -- u / u       = 1
  Div  u         v        = u :/: v         -- u / v       = u / v


-- * Calculation

-- | Compute the square of the distance efficiently and in the correct dimensions.
qdU :: (Metric v, Unit du u, Num a) => v (u a) -> v (u a) -> (u :*: u) a
u `qdU` v = pure $ fmap prj u `qd` fmap prj v


-- * Combinators

newtype ((u :: * -> *) :*: (v :: * -> *)) a = Prd { getPrd :: a }
  deriving (Column, Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

infixl 7 :*:

instance (Unit dimu u, Unit dimv v) => Unit (dimu :*: dimv) (u :*: v) where
  factor = K (getK (factor @_ @u) * getK (factor @_ @v))
  suffix = K (getK (suffix @_ @u) . ('·' :) . getK (suffix @_ @v))


newtype ((u :: * -> *) :/: (v :: * -> *)) a = Per { getPer :: a }
  deriving (Column, Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

infixl 7 :/:

instance (Unit dimu u, Unit dimv v) => Unit (dimu :/: dimv) (u :/: v) where
  factor = K (getK (factor @_ @u) / getK (factor @_ @v))
  suffix = K (getK (suffix @_ @u) . ('/' :) . getK (suffix @_ @v))


type family u :^: n where
  _ :^: 0 = I
  u :^: 1 = u
  u :^: n = u :*: u :^: (n - 1)

infixr 8 :^:
