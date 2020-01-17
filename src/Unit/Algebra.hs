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
, (:/:)(..)
, Inv
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

(.*.) :: (Unit du u, Unit dv v, Unit dw (Simplify (u :*: v)), Fractional a) => u a -> v a -> Simplify (u :*: v) a
u .*. v = pure (prj v * prj u)

infixl 7 .*.

(./.) :: (Unit du u, Unit dv v, Unit dw (Simplify (u :/: v)), Fractional a) => u a -> v a -> Simplify (u :/: v) a
u ./. v = pure (prj u / prj v)

infixl 7 ./.

type family Simplify u where
  Simplify (I       :*: u) = u
  Simplify (u       :*: I) = u
  Simplify (u       :/: I) = u
  Simplify (u :/: v :*: v) = u
  Simplify (u :*: v :/: v) = u
  Simplify (v :*: u :/: v) = u
  Simplify (u :*: v :*: w) = Simplify (u :*: w) :*: v
  Simplify (u :*: v :/: w) = Simplify (u :/: w) :*: v
  Simplify (u :/: v :*: w) = Simplify (u :*: w) :/: v
  Simplify (u :/: v :/: w) = Simplify (u :/: w) :/: v
  Simplify (u :*: (v :*: w)) = Simplify (u :*: v :*: w)
  Simplify (u :/: (v :*: w)) = Simplify (u :/: v :/: w)
  Simplify u               = u


-- * Combinators

newtype ((u :: * -> *) :*: (v :: * -> *)) a = Prd { getPrd :: a }
  deriving (Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Scalar, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via Identity

infixl 7 :*:

instance (Unit dimu u, Unit dimv v) => Unit (dimu :*: dimv) (u :*: v) where
  factor = Const (getConst (factor @_ @u) * getConst (factor @_ @v))
  suffix = Const (getConst (suffix @_ @u) . ('·' :) . getConst (suffix @_ @v))


newtype ((u :: * -> *) :/: (v :: * -> *)) a = Per { getPer :: a }
  deriving (Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Scalar, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via Identity

infixl 7 :/:

instance (Unit dimu u, Unit dimv v) => Unit (dimu :/: dimv) (u :/: v) where
  factor = Const (getConst (factor @_ @u) / getConst (factor @_ @v))
  suffix = Const (getConst (suffix @_ @u) . ('/' :) . getConst (suffix @_ @v))


type Inv u = I :/: u


type I = Identity

pattern I :: a -> Identity a
pattern I a = Identity a

getI :: I a -> a
getI = runIdentity
{-# INLINABLE getI #-}

{-# COMPLETE I #-}
