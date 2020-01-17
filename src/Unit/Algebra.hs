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
  Alg(..)
, (./.)
  -- * Combinators
, (:*:)(..)
, (:/:)(..)
, Inv(..)
, I
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

class (Unit dimu u, Unit dimv v) => Alg dimu u dimv v | u -> dimu, v -> dimv where
  type Res u v (c :: (* -> *) -> (* -> *) -> (* -> *)) :: * -> *
  (.*.) :: Fractional a => u a -> v a -> Res u v (:*:) a

infixl 7 .*.

(./.) :: forall dimu u dimv v a . (Alg dimu u dimv (Inv v), Unit dimv v, Fractional a) => u a -> v a -> Res u (Inv v) (:*:) a
u ./. v = u .*. Inv @v (1/prj v)

infixl 7 ./.

instance (AlgBy step dimu u dimv v, Step u v ~ step, Unit dimu u, Unit dimv v) => Alg dimu u dimv v where
  type Res u v c = ResBy (Step u v) u v c
  (.*.) = mulBy @step


class (Unit dimu u, Unit dimv v) => AlgBy (step :: Act) dimu u dimv v | u -> dimu, v -> dimv where
  type ResBy step u v (c :: (* -> *) -> (* -> *) -> (* -> *)) :: * -> *
  mulBy :: Fractional a => u a -> v a -> ResBy step u v (:*:) a

-- | Prepend at the head of the chain.
instance {-# OVERLAPPABLE #-} (Unit dimu u, Unit dimv v) => AlgBy 'Prepend dimu u dimv v where
  type ResBy 'Prepend u v (:*:) = u :*: v
  u `mulBy` v = Prd (prj u * prj v)

-- | Elimination by reciprocals at left.
instance {-# OVERLAPPABLE #-} (Unit dimu u, Unit dimv v, Unit dimu' u', u' ~ InvOf u) => AlgBy 'CancelL (dimu :*: dimv) (u :*: v) dimu' u' where
  type ResBy 'CancelL (u :*: v) u' (:*:) = v
  u `mulBy` v = pure (prj u * prj v)

-- | Elimination by reciprocals at right.
instance {-# OVERLAPPABLE #-} (Unit dimu u, Unit dimv v, Unit dimv' v', v' ~ InvOf v) => AlgBy 'CancelR (dimu :*: dimv) (u :*: v) dimv' v' where
  type ResBy 'CancelR (u :*: v) v' (:*:) = u
  u `mulBy` v = pure (prj u * prj v)

-- | Decompose products on the right.
instance {-# OVERLAPPABLE #-} (Alg dimu u dimv' v', Alg dimuv' (Res u v' (:*:)) dimv v, Unit dimv' v', Unit dimuv'v (Res (Res u v' (:*:)) v (:*:))) => AlgBy 'Decompose dimu u (dimv :*: dimv') (v :*: v') where
  type ResBy 'Decompose u (v :*: v') (:*:) = Res (Res u v' (:*:)) v (:*:)
  u `mulBy` Prd v = pure (prj u * v)

-- | Walk the chain.
instance {-# OVERLAPPABLE #-} (Alg dimu u dimv v, Unit dimu' u') => AlgBy 'Walk (dimu :*: dimu') (u :*: u') dimv v where
  type ResBy 'Walk (u :*: u') v (:*:) = Res u v (:*:) :*: u'
  Prd u `mulBy` v = Prd (u * prj v)


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


type I = Identity
