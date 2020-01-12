{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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

class (Unit u, Unit v, Unit w) => Mul u v w where
  (.*.) :: Fractional a => u a -> v a -> w a

infixl 7 .*.

(./.) :: forall u v w a . (Mul u (Inv v) w, Unit v, Fractional a) => u a -> v a -> w a
u ./. v = u .*. Inv @v (negate (prj v))

infixl 7 ./.

-- | Append at the head of the chain.
instance {-# OVERLAPPABLE #-} (Unit u, Unit v) => Mul u v (u :*: v) where
  u .*. v = Prd (prj u * prj v)

-- | Elimination by reciprocals.
instance {-# OVERLAPPABLE #-} (Unit u, Unit v) => Mul (u :*: v) (Inv v) u where
  u .*. v = pure (prj u * prj v)

-- | Elimination of reciprocals.
instance {-# OVERLAPPABLE #-} (Unit u, Unit v) => Mul (u :*: Inv v) v u where
  u .*. v = pure (prj u * prj v)

-- | Walk the chain.
instance {-# OVERLAPPABLE #-} (Mul u v w, Unit u') => Mul (u :*: u') v (w :*: u') where
  Prd u .*. v = Prd (u * prj v)


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
