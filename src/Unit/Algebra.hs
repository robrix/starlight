{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Unit.Algebra
(  -- * Algebra
  (.*.)
, (./.)
  -- * Combinators
, (:*:)(..)
, (:/:)
, Inv(..)
, I
, pattern I
, getI
, Identity(..)
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

-- * Algebra

(.*.) :: (Unit du u, Unit dv v, Unit dw (Mul u v), Fractional a) => u a -> v a -> Mul u v a
u .*. v = pure (prj v * prj u)

infixl 7 .*.

(./.) :: (Unit du u, Unit dv v, Unit dw (Mul u (Inv v)), Fractional a) => u a -> v a -> Mul u (Inv v) a
u ./. v = pure (prj u / prj v)

infixl 7 ./.

type family Mul u v where
  Mul  u                  I         = u                           -- u * 1       = u
  Mul  I                  v         = v                           -- 1 * v       = v
  Mul  u            (Inv  I)        = u                           -- u / 1       = u
  Mul (u :*: Inv v)       v         = u                           -- u / v * v   = u
  Mul (u :*: v)     (Inv  v)        = u                           -- u * v / v   = u
  Mul (u :*: v)     (Inv  u)        = v                           -- u * v / u   = v
  Mul  u                 (v :*: w)  = Mul (Mul u w) v             -- u * (v * w) = (u * w) * v
  Mul  u            (Inv (v :*: w)) = Mul (Mul u (Inv w)) (Inv v) -- u / (v * w) = (u / w) / v
  Mul (u :*: v)           w         = Mul u w :*: v               -- (u * v) * w = (u * w) * v
  Mul  u                  v         = u :*: v                     -- u * v       = u * v


-- * Combinators

newtype ((u :: * -> *) :*: (v :: * -> *)) a = Prd { getPrd :: a }
  deriving (Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Scalar, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via Identity

infixl 7 :*:

instance (Unit dimu u, Unit dimv v) => Unit (dimu :*: dimv) (u :*: v) where
  factor = Const (getConst (factor @_ @u) * getConst (factor @_ @v))
  suffix = Const (getConst (suffix @_ @u) . ('·' :) . getConst (suffix @_ @v))


type u :/: v = u :*: Inv v

infixl 7 :/:


newtype Inv (u :: * -> *) a = Inv { getInv :: a }
  deriving (Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Scalar, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via Identity

instance Unit dimu u => Unit dimu (Inv u) where
  prj = getInv
  factor = Const (1/getConst (factor @_ @u))
  suffix = Const (getConst (suffix @_ @u) . ('⁻' :) . ('¹' :))


type I = Identity

pattern I :: a -> Identity a
pattern I a = Identity a

getI :: I a -> a
getI = runIdentity
{-# INLINABLE getI #-}

{-# COMPLETE I #-}
