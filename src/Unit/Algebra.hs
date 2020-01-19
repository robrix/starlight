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

(.*.) :: (Unit du u, Unit dv v, Unit dw (Mul u v), Fractional a) => u a -> v a -> Mul u v a
u .*. v = pure (prj v * prj u)

infixl 7 .*.

(./.) :: (Unit du u, Unit dv v, Unit dw (Div u v), Fractional a) => u a -> v a -> Div u v a
u ./. v = pure (prj u / prj v)

infixl 7 ./.

type family Mul u v where
  Mul  u         I        = u               -- u * 1       = u
  Mul  I         v        = v               -- 1 * v       = v
  Mul (u :^: n)  u        = u :^: (n + 1)   -- uⁿ * u      = uⁿ⁺¹
  Mul (u :^: i) (u :^: n) = u :^: (i + n)   -- uⁱ * uⁿ     = uⁱ⁺ⁿ
  Mul  u         u        = u :^: 2         -- u * u       = u²
  Mul (u :/: v)  v        = u               -- u / v * v   = u
  Mul  u        (v :*: w) = Mul (Mul u w) v -- u * (v * w) = (u * w) * v
  Mul  u        (v :/: w) = Div (Mul u v) w -- u * (v / w) = (u * v) / w
  Mul (u :*: v)  w        = Mul u w :*: v   -- (u * v) * w = (u * w) * v
  Mul (u :/: v)  w        = Mul u w :/: v   -- (u / v) * w = (u * w) / v
  Mul  u         v        = u :*: v         -- u * v       = u * v

type family Div u v where
  Div  u         I        = u               -- u / 1       = u
  Div  u         u        = I               -- u / u       = 1
  Div (u :*: v)  v        = u               -- u * v / v   = u
  Div (u :*: v)  u        = v               -- u * v / u   = v
  Div (u :^: n)  u        = Exp u (n - 1)   -- uⁿ / u      = uⁿ⁻¹
  Div  u        (v :*: w) = Div (Div u w) v -- u / (v * w) = (u / w) / v
  Div (u :*: v)  w        = Mul (Div u w) v -- (u * v) / w = (u / w) * v
  Div (u :/: v)  w        = Div u w :/: v   -- (u / v) / w = (u / w) / v
  Div  u         v        = u :/: v         -- u / v       = u / v

type family Exp u n where
  Exp _ 0 = I
  Exp u 1 = u
  Exp u n = u :^: n


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


newtype ((u :: * -> *) :^: (n :: Nat)) a = Exp { getExp :: a }
  deriving (Column, Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

infixr 8 :^:

instance (Unit dimu u, KnownNat n) => Unit (dimu :^: n) (u :^: n) where
  factor = K (getK (factor @_ @u) ^ natVal (Proxy @n))
  suffix = K (getK (suffix @_ @u) . superscript (fromIntegral (natVal (Proxy @n))))
