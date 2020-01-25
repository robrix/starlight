{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Unit.Algebra
(  -- * Algebra
  (.*.)
, (^*.)
, (.*^)
, (./.)
, (^/.)
, (./^)
, Mul
, Div
, Exp
, Dimension
, Pow
  -- * Calculation
, sqU
, sqrtU
, dotU
, quadranceU
, qdU
, normalizeU
  -- * Combinators
, (:*:)(..)
, (:/:)(..)
, (:^:)
, I(..)
) where

import Control.Monad (join)
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
import System.Random (Random)
import Unit

-- * Algebra

(.*.) :: (Unit du u, Unit dv v, Unit d' (Mul u v), Num a) => u a -> v a -> Mul u v a
u .*. v = pure (prj u * prj v)

infixl 7 .*.

(^*.) :: (Functor f, Unit du u, Unit dv v, Unit d' (Mul u v), Num a) => f (u a) -> v a -> f (Mul u v a)
u ^*. v = pure . (* prj v) . prj <$> u

infixl 7 ^*.

(.*^) :: (Unit du u, Functor f, Unit dv v, Unit d' (Mul u v), Num a) => u a -> f (v a) -> f (Mul u v a)
u .*^ v = pure . (prj u *) . prj <$> v

infixl 7 .*^

(./.) :: (Unit du u, Unit dv v, Unit d' (Div u v), Fractional a) => u a -> v a -> Div u v a
u ./. v = pure (prj u / prj v)

infixl 7 ./.

(^/.) :: (Functor f, Unit du u, Unit dv v, Unit d' (Div u v), Fractional a) => f (u a) -> v a -> f (Div u v a)
u ^/. v = pure . (/ prj v) . prj <$> u

infixl 7 ^/.

(./^) :: (Unit du u, Functor f, Unit dv v, Unit d' (Div u v), Fractional a) => u a -> f (v a) -> f (Div u v a)
u ./^ v = pure . (prj u /) . prj <$> v

infixl 7 ./^


type family Mul u v where
  Mul  u               I        = u                             -- u * 1       = u
  Mul  I               v        = v                             -- 1 * v       = v
  Mul (u :^: n)        u        = u :^: (n + 1)                 -- uⁿ * u      = uⁿ⁺¹
  Mul (u :^: i)       (u :^: n) = u :^: (i + n)                 -- uⁱ * uⁿ     = uⁱ⁺ⁿ
  Mul  u               u        = Exp u 2                       -- u * u       = u²
  Mul (u :/: v)        v        = u                             -- u / v * v   = u
  Mul (u :/: v :^: n)  v        = Div u (Exp v (n - 1))         -- u / vⁿ * v  = u / vⁿ⁻¹
  Mul  u              (v :*: w) = Mul (Mul u w) v               -- u * (v * w) = (u * w) * v
  Mul  u              (v :^: n) = Mul (Mul u v) (Exp v (n - 1)) -- u * vⁿ      = (u * v) * vⁿ⁻¹
  Mul  u              (v :/: w) = Div (Mul u v) w               -- u * (v / w) = (u * v) / w
  Mul (u :*: v)        w        = Mul u w :*: v                 -- (u * v) * w = (u * w) * v
  Mul (u :/: v)        w        = Mul u w :/: v                 -- (u / v) * w = (u * w) / v
  Mul  u               v        = u :*: v                       -- u * v       = u * v

-- FIXME: can we simplify walking on right?
type family Div u v where
  Div  u               u        = I                             -- u / u       = 1
  Div  u               I        = u                             -- u / 1       = u
  Div (u :*: v)        v        = u                             -- u * v / v   = u
  Div (u :^: n)        u        = Exp u (n - 1)                 -- uⁿ / u      = uⁿ⁻¹
  Div (u :*: v :^: n)  v        = u :*: Exp v (n - 1)           -- u / vⁿ / v  = u / vⁿ⁺¹
  Div (u :/: v :^: n)  v        = u :/: v :^: (n + 1)           -- u / vⁿ / v  = u / vⁿ⁺¹
  Div  u              (v :*: w) = Div (Div u w) v               -- u / (v * w) = (u / w) / v
  Div  u              (v :^: n) = Div (Div u v) (Exp v (n - 1)) -- u / vⁿ      = (u / v) / vⁿ⁻¹
  Div  u              (v :/: w) = Mul (Div u v) w               -- u / (v / w) = u / v * w
  Div (u :*: v)        w        = Mul (Div u w) v               -- (u * v) / w = (u / w) * v
  Div (u :/: v)        v        = u :/: Exp v 2                 -- (u / v) / v = u / v²
  Div (u :/: v)        w        = Div u w :/: v                 -- (u / v) / w = (u / w) / v
  Div  u               v        = u :/: v                       -- u / v       = u / v

type family Exp u n where
  Exp I _ = I
  Exp _ 0 = I
  Exp u 1 = u
  Exp u n = u :^: n


class Dimension (dim :: * -> *)

instance Dimension I


class (Dimension du, Unit du u, Unit du' u', KnownNat n) => Pow du du' u (n :: Nat) u' | du u n -> du' u', du du' u' n -> u, du' u' u n -> du, du n -> du'

instance (Unit I u, KnownNat n) => Pow I I u n u


-- * Calculation

sqU :: (Pow du dsqu u 2 squ, Num a) => u a -> squ a
sqU = pure . join (*) . prj

sqrtU :: (Pow dsqrtu du sqrtu 2 u, Floating a) => u a -> sqrtu a
sqrtU = pure . sqrt . prj

dotU :: (Num a, Metric v, Unit d u) => v (u a) -> v (u a) -> u a
dotU a b = pure (dot (prj <$> a) (prj <$> b))

-- | Compute the square of the norm efficiently and in the correct dimensions.
quadranceU :: (Metric v, Unit d u, Num a) => v (u a) -> (u :^: 2) a
quadranceU = pure . quadrance . fmap prj

-- | Compute the square of the distance efficiently and in the correct dimensions.
qdU :: (Metric v, Unit d u, Num a) => v (u a) -> v (u a) -> (u :^: 2) a
u `qdU` v = pure $ fmap prj u `qd` fmap prj v

normalizeU :: (Metric v, Unit d u, Floating a, Epsilon a) => v (u a) -> v (I a)
normalizeU = fmap I . normalize . fmap prj


-- * Combinators

newtype ((u :: * -> *) :*: (v :: * -> *)) a = Prd { getPrd :: a }
  deriving (Column, Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Random, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

infixl 7 :*:

instance Dimension (du :*: dv)

instance (Pow du dun u n un, Pow dv dvn v n vn) => Pow (du :*: dv) (dun :*: dvn) (u :*: v) n (un :*: vn)

instance (Unit du u, Unit dv v) => Unit (du :*: dv) (u :*: v) where
  factor = K (getK (factor @_ @u) * getK (factor @_ @v))
  suffix = K (getK (suffix @_ @u) . ('·' :) . getK (suffix @_ @v))


newtype ((u :: * -> *) :/: (v :: * -> *)) a = Per { getPer :: a }
  deriving (Column, Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Random, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

infixl 7 :/:

instance Dimension (du :/: dv)

instance (Pow du dun u n un, Pow dv dvn v n vn) => Pow (du :/: dv) (dun :/: dvn) (u :/: v) n (un :/: vn)

instance (Unit du u, Unit dv v) => Unit (du :/: dv) (u :/: v) where
  factor = K (getK (factor @_ @u) / getK (factor @_ @v))
  suffix = K (getK (suffix @_ @u) . ('/' :) . getK (suffix @_ @v))


newtype ((u :: * -> *) :^: (n :: Nat)) a = Exp { getExp :: a }
  deriving (Column, Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Random, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

infixr 8 :^:

instance Dimension (du :^: m)

instance (Unit du u, Plus m n o) => Pow (du :^: m) (du :^: o) (u :^: m) n (u :^: o)

instance (Unit du u, KnownNat n) => Unit (du :^: n) (u :^: n) where
  factor = K (getK (factor @_ @u) ^ natVal (Proxy @n))
  suffix = K (getK (suffix @_ @u) . superscript (fromIntegral (natVal (Proxy @n))))


-- | Inductive, 'Nat'-indexed naturals, used for injective operations on 'Nat'.
--
-- The type families for operations on 'Nat' are non-injective, and thus we have to resort to cleverness here. Inductive definitions of the naturals are standard, while the type index allows us to inductively relate a 'Nat' to the corresponding 'N' without losing injectivity.
data N (n :: Nat) where
  Z :: N 0
  S :: N (n - 1) -> N n


-- | Select the 'N' corresponding to a given 'Nat', injectively.
type family FromNat (n :: Nat) = (n' :: N n) | n' -> n where
  FromNat 0 = 'Z
  FromNat n = 'S (FromNat (n - 1))


-- | Three-way injective addition (and thus also subtraction) of 'Nat's.
--
-- The technique for three-way injectivity is due to Oleg: http://archive.md/JwMNI
class (KnownNat a, KnownNat b, KnownNat c) => Plus (a :: Nat) (b :: Nat) (c :: Nat) | a b -> c, a c -> b, b c -> a
instance (KnownNat a, KnownNat b, KnownNat c, Plus' (FromNat a) (FromNat b) (FromNat c), Plus' (FromNat b) (FromNat a) (FromNat c)) => Plus a b c

class Plus' (a :: N na) (b :: N nb) (c :: N nc) | a b -> c, a c -> b
instance Plus' 'Z n n
instance Plus' m n o
      => Plus' ('S m) n ('S o)
