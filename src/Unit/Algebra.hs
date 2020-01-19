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
, N(..)
, (.^.)
  -- * Calculation
, sqrtU
, qdU
, normU
  -- * Combinators
, (:*:)(..)
, (:/:)(..)
, (:^:)
, I(..)
) where

import Data.Functor.I
import Data.Functor.K
import Data.Proxy
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

(.*.) :: (Unit u, Unit v, Unit (Mul u v), Fractional a) => u a -> v a -> Mul u v a
u .*. v = pure (prj v * prj u)

infixl 7 .*.

(./.) :: (Unit u, Unit v, Unit (Div u v), Fractional a) => u a -> v a -> Div u v a
u ./. v = pure (prj u / prj v)

infixl 7 ./.

data N (n :: Nat) = N

(.^.) :: (Unit u, Unit (Exp u n), Fractional a, KnownNat n) => u a -> N n -> Exp u n a
u .^. n = pure (prj u ^ natVal n)

infixr 8 .^.

type family Mul u v where
  Mul  u               I        = u                             -- u * 1       = u
  Mul  I               v        = v                             -- 1 * v       = v
  Mul (u :^: n)        u        = u :^: (n + 1)                 -- uⁿ * u      = uⁿ⁺¹
  Mul (u :^: i)       (u :^: n) = u :^: (i + n)                 -- uⁱ * uⁿ     = uⁱ⁺ⁿ
  Mul  u               u        = u :^: 2                       -- u * u       = u²
  Mul (u :/: v)        v        = u                             -- u / v * v   = u
  Mul (u :/: v :^: n)  v        = Div u (Exp v (n - 1))         -- u / vⁿ * v   = u / vⁿ⁻¹
  Mul  u              (v :*: w) = Mul (Mul u w) v               -- u * (v * w) = (u * w) * v
  Mul  u              (v :^: n) = Mul (Mul u v) (Exp v (n - 1)) -- u * (v / w) = (u * v) / w
  Mul  u              (v :/: w) = Div (Mul u v) w               -- u * (v / w) = (u * v) / w
  Mul (u :*: v)        w        = Mul u w :*: v                 -- (u * v) * w = (u * w) * v
  Mul (u :/: v)        w        = Mul u w :/: v                 -- (u / v) * w = (u * w) / v
  Mul  u               v        = u :*: v                       -- u * v       = u * v

-- FIXME: can we simplify walking on right?
type family Div u v where
  Div  u               I        = u                             -- u / 1       = u
  Div  u               u        = I                             -- u / u       = 1
  Div (u :*: v)        v        = u                             -- u * v / v   = u
  Div (u :^: n)        u        = Exp u (n - 1)                 -- uⁿ / u      = uⁿ⁻¹
  Div (u :*: v :^: n)  v        = u :*: Exp v (n - 1)           -- u / vⁿ / v  = u / vⁿ⁺¹
  Div (u :/: v :^: n)  v        = u :/: v :^: (n + 1)           -- u / vⁿ / v  = u / vⁿ⁺¹
  Div  u              (v :*: w) = Div (Div u w) v               -- u / (v * w) = (u / w) / v
  Div  u              (v :^: n) = Div (Div u v) (Exp v (n - 1)) -- u / (v * w) = (u / w) / v
  Div (u :*: v)        w        = Mul (Div u w) v               -- (u * v) / w = (u / w) * v
  Div (u :/: v)        v        = u :/: v :^: 2                 -- (u / v) / v = u / v²
  Div (u :/: v)        w        = Div u w :/: v                 -- (u / v) / w = (u / w) / v
  Div  u               v        = u :/: v                       -- u / v       = u / v

type family Exp u n where
  Exp I _ = I
  Exp _ 0 = I
  Exp u 1 = u
  Exp u n = u :^: n

type family Sqrt u where
  Sqrt (u :*: v) = Sqrt u :*: Sqrt v
  Sqrt (u :/: v) = Sqrt u :/: Sqrt v
  Sqrt (u :^: 2) = u
  Sqrt (u :^: 4) = u :^: 2


-- * Calculation

sqrtU :: (Unit u, Unit (Sqrt u), Floating a) => u a -> Sqrt u a
sqrtU = pure . sqrt . prj

-- | Compute the square of the distance efficiently and in the correct dimensions.
qdU :: (Metric v, Unit u, Num a) => v (u a) -> v (u a) -> (u :^: 2) a
u `qdU` v = pure $ fmap prj u `qd` fmap prj v

normU :: (Metric v, Unit u, Floating a) => v (u a) -> I a
normU = I . norm . fmap prj


-- * Combinators

newtype ((u :: * -> *) :*: (v :: * -> *)) a = Prd { getPrd :: a }
  deriving (Column, Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

infixl 7 :*:

instance (Unit u, Unit v) => Unit (u :*: v) where
  type Dim (u :*: v) = Dim u :*: Dim v
  factor = K (getK (factor @u) * getK (factor @v))
  suffix = K (getK (suffix @u) . ('·' :) . getK (suffix @v))


newtype ((u :: * -> *) :/: (v :: * -> *)) a = Per { getPer :: a }
  deriving (Column, Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

infixl 7 :/:

instance (Unit u, Unit v) => Unit (u :/: v) where
  type Dim (u :/: v) = Dim u :/: Dim v
  factor = K (getK (factor @u) / getK (factor @v))
  suffix = K (getK (suffix @u) . ('/' :) . getK (suffix @v))


newtype ((u :: * -> *) :^: (n :: Nat)) a = Exp { getExp :: a }
  deriving (Column, Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

infixr 8 :^:

instance (Unit u, KnownNat n) => Unit (u :^: n) where
  type Dim (u :^: n) = Dim u :^: n
  factor = K (getK (factor @u) ^ natVal (Proxy @n))
  suffix = K (getK (suffix @u) . superscript (fromIntegral (natVal (Proxy @n))))
