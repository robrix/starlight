{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Unit.Algebra
(  -- * Algebra
  Mul(..)
, (./.)
  -- * Combinators
, (:*:)(..)
, (:/:)(..)
, Inv(..)
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

class (Unit dimu u, Unit dimv v) => Mul dimu u dimv v | u -> dimu, v -> dimv where
  type Res u v :: * -> *
  (.*.) :: Fractional a => u a -> v a -> Res u v a

infixl 7 .*.

(./.) :: forall dimu u dimv v a . (Mul dimu u dimv (Inv v), Unit dimv v, Fractional a) => u a -> v a -> Res u (Inv v) a
u ./. v = u .*. Inv @v (1/prj v)

infixl 7 ./.

instance (MulBy step dimu u dimv v, Step u v ~ step, Unit dimu u, Unit dimv v) => Mul dimu u dimv v where
  type Res u v = ResBy (Step u v) u v
  (.*.) = mulBy @step


class (Unit dimu u, Unit dimv v) => MulBy (step :: Act) dimu u dimv v where
  type ResBy step u v :: * -> *
  mulBy :: Fractional a => u a -> v a -> ResBy step u v a

-- | Prepend at the head of the chain.
instance {-# OVERLAPPABLE #-} (Unit dimu u, Unit dimv v) => MulBy 'Prepend dimu u dimv v where
  type ResBy 'Prepend u v = u :*: v
  u `mulBy` v = Prd (prj u * prj v)

-- | Elimination by reciprocals at left.
instance {-# OVERLAPPABLE #-} (Unit dimu u, Unit dimv v, Unit dimu' u', u' ~ InvOf u) => MulBy 'CancelL (dimu :*: dimv) (u :*: v) dimu' u' where
  type ResBy 'CancelL (u :*: v) u' = v
  u `mulBy` v = pure (prj u * prj v)

-- | Elimination by reciprocals at right.
instance {-# OVERLAPPABLE #-} (Unit dimu u, Unit dimv v, Unit dimv' v', v' ~ InvOf v) => MulBy 'CancelR (dimu :*: dimv) (u :*: v) dimv' v' where
  type ResBy 'CancelR (u :*: v) v' = u
  u `mulBy` v = pure (prj u * prj v)

-- | Decompose products on the right.
instance {-# OVERLAPPABLE #-} (Mul dimu u dimv' v', Mul dimuv' (Res u v') dimv v, Unit dimv' v', Unit dimuv'v (Res (Res u v') v)) => MulBy 'Decompose dimu u (dimv :*: dimv') (v :*: v') where
  type ResBy 'Decompose u (v :*: v') = Res (Res u v') v
  u `mulBy` Prd v = pure (prj u * v)

-- | Walk the chain.
instance {-# OVERLAPPABLE #-} (Mul dimu u dimv v, Unit dimu' u') => MulBy 'Walk (dimu :*: dimu') (u :*: u') dimv v where
  type ResBy 'Walk (u :*: u') v = Res u v :*: u'
  Prd u `mulBy` v = Prd (u * prj v)


type family Mul' u v where
  Mul' v         Identity = v
  Mul' Identity  v        = v
  Mul' (u :/: v) v        = u
  Mul' u         v        = u :*: v

type family Div' u v where
  Div' (u :*: v) u = v
  Div' (u :*: v) v = u
  Div' u         v = u :/: v

data Act = Prepend | CancelL | CancelR | Decompose | Walk

type family Step u v where
  Step (v     :*: _) (Inv v)   = 'CancelL
  Step (Inv v :*: _) v         = 'CancelL
  Step (_     :*: v) (Inv v)   = 'CancelR
  Step (_     :/: v) v         = 'CancelR
  Step _             (_ :*: _) = 'Decompose
  Step (_     :*: _) _         = 'Walk
  Step _             _         = 'Prepend

type family InvOf u where
  InvOf (Inv u) = u
  InvOf u       = Inv u


-- * Combinators

newtype ((u :: * -> *) :*: (v :: * -> *)) a = Prd { getPrd :: a }
  deriving (Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Scalar, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via Identity

infixl 7 :*:

instance (Unit dimu u, Unit dimv v) => Unit (dimu :*: dimv) (u :*: v) where
  factor = Const (getConst (factor @_ @u) * getConst (factor @_ @v))
  suffix = Const (getConst (suffix @_ @u) . ('·' :) . getConst (suffix @_ @v))


newtype Inv (u :: * -> *) a = Inv { getInv :: a }
  deriving (Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Scalar, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via Identity

instance Unit dimu u => Unit dimu (Inv u) where
  prj = getInv
  factor = Const (1/getConst (factor @_ @u))
  suffix = Const (getConst (suffix @_ @u) . ('⁻' :) . ('¹' :))


newtype ((u :: * -> *) :/: (v :: * -> *)) a = Per { getPer :: a }
  deriving (Conjugate, Epsilon, Enum, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Scalar, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via Identity

infixl 7 :/:

instance (Unit dimu u, Unit dimv v) => Unit (dimu :/: dimv) (u :/: v) where
  factor = Const (getConst (factor @_ @u) / getConst (factor @_ @v))
  suffix = Const (getConst (suffix @_ @u) . ('/' :) . getConst (suffix @_ @v))
