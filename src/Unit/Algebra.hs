{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
, Inv(..)
, (:/:)
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

class (Unit u, Unit v) => Mul u v where
  type Res u v :: * -> *
  (.*.) :: Fractional a => u a -> v a -> Res u v a

infixl 7 .*.

(./.) :: forall u v a . (Mul u (Inv v), Unit v, Fractional a) => u a -> v a -> Res u (Inv v) a
u ./. v = u .*. Inv @v (1/prj v)

infixl 7 ./.

instance (MulBy step u v, Step u v ~ step, Unit u, Unit v) => Mul u v where
  type Res u v = ResBy (Step u v) u v
  (.*.) = mulBy @step


class (Unit u, Unit v) => MulBy (step :: Act) u v where
  type ResBy step u v :: * -> *
  mulBy :: Fractional a => u a -> v a -> ResBy step u v a

-- | Append at the head of the chain.
instance {-# OVERLAPPABLE #-} (Unit u, Unit v) => MulBy 'Prepend u v where
  type ResBy 'Prepend u v = u :*: v
  u `mulBy` v = Prd (prj u * prj v)

-- | Elimination by reciprocals at left.
instance {-# OVERLAPPABLE #-} (Unit u, Unit v, Unit u', u' ~ InvOf u) => MulBy 'CancelL (u :*: v) u' where
  type ResBy 'CancelL (u :*: v) u' = v
  u `mulBy` v = pure (prj u * prj v)

-- | Elimination by reciprocals at right.
instance {-# OVERLAPPABLE #-} (Unit u, Unit v, Unit v', v' ~ InvOf v) => MulBy 'CancelR (u :*: v) v' where
  type ResBy 'CancelR (u :*: v) v' = u
  u `mulBy` v = pure (prj u * prj v)

-- | Decompose products on the right.
instance {-# OVERLAPPABLE #-} (Mul u v', Mul (Res u v') v, Unit v', Unit (Res (Res u v') v)) => MulBy 'Decompose u (v :*: v') where
  type ResBy 'Decompose u (v :*: v') = Res (Res u v') v
  u `mulBy` Prd v = pure (prj u * v)

-- | Walk the chain.
instance {-# OVERLAPPABLE #-} (Mul u v, Unit u') => MulBy 'Walk (u :*: u') v where
  type ResBy 'Walk (u :*: u') v = Res u v :*: u'
  Prd u `mulBy` v = Prd (u * prj v)


data Act = Prepend | CancelL | CancelR | Decompose | Walk

type family Step u v where
  Step (v     :*: _)     (Inv v)   = 'CancelL
  Step (Inv v :*: _)     v         = 'CancelL
  Step (_     :*: v)     (Inv v)   = 'CancelR
  Step (_     :/: v) v         = 'CancelR
  Step _                 (_ :*: _) = 'Decompose
  Step (_     :*: _)     _         = 'Walk
  Step _                 _         = 'Prepend

type family InvOf u where
  InvOf (Inv u) = u
  InvOf u       = Inv u


-- * Combinators

newtype ((u :: * -> *) :*: (v :: * -> *)) a = Prd { getPrd :: a }
  deriving (Conjugate, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via Identity

infixl 7 :*:

instance (Unit u, Unit v) => Unit (u :*: v) where
  factor = Const (getConst (factor @u) * getConst (factor @v))
  suffix = Const (getConst (suffix @u) . ('·' :) . getConst (suffix @v))


newtype Inv (u :: * -> *) a = Inv { getInv :: a }
  deriving (Conjugate, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via Identity

instance Unit u => Unit (Inv u) where
  prj = getInv
  factor = Const (1/getConst (factor @u))
  suffix = Const (getConst (suffix @u) . ('⁻' :) . ('¹' :))


type (u :/: v) = u :*: Inv v

infixl 7 :/:
