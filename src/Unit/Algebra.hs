{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Unit.Algebra
(  -- * Algebra
  Mul(..)
, Div(..)
  -- * Combinators
, (:/:)(..)
, (:*:)(..)
) where

import Control.Applicative (liftA2)
import Data.Functor.Const
import Data.Functor.Identity
import Foreign.Storable
import GHC.Generics ((:.:)(..))
import GL.Type as GL
import GL.Uniform
import Linear.Metric
import Linear.Vector
import Unit

-- * Algebra

class (Applicative u, Applicative v, Applicative w) => Mul u v w | u v -> w, u w -> v, v w -> u where
  (.*.) :: Fractional a => u a -> v a -> w a

class (Applicative u, Applicative v, Applicative w) => Div u v w | u v -> w, u w -> v, v w -> u where
  (./.) :: Fractional a => u a -> v a -> w a

instance Mul Identity Identity Identity where
  (.*.) = (*)

instance Div Identity Identity Identity where
  (./.) = (/)


-- * Combinators

newtype (u :*: v) a = Prd { getPrd :: u (v a) }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Applicative via u :.: v

infixl 7 :*:

instance (Applicative u, Additive v) => Additive (u :*: v) where
  zero = Prd (pure zero)
  liftU2 f (Prd a) (Prd b) = Prd (liftA2 (liftU2 f) a b)
  liftI2 f (Prd a) (Prd b) = Prd (liftA2 (liftI2 f) a b)

instance (Applicative u, Foldable u, Additive v, Foldable v) => Metric (u :*: v)

instance (Unit u, Unit v) => Unit (u :*: v) where
  prj = prj . prj . getPrd
  factor = Const (getConst (factor @u) * getConst (factor @v))
  suffix = Const (getConst (suffix @u) . ('·' :) . getConst (suffix @v))


newtype (u :/: v) a = Per { getPer :: u (v a) }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Applicative via u :.: v

infixl 7 :/:

instance (Applicative u, Additive v) => Additive (u :/: v) where
  zero = Per (pure zero)
  liftU2 f (Per a) (Per b) = Per (liftA2 (liftU2 f) a b)
  liftI2 f (Per a) (Per b) = Per (liftA2 (liftI2 f) a b)

instance (Applicative u, Foldable u, Additive v, Foldable v) => Metric (u :/: v)

instance (Unit u, Unit v) => Unit (u :/: v) where
  prj = prj . prj . getPer
  factor = Const (getConst (factor @v) / getConst (factor @u))
  suffix = Const (getConst (suffix @u) . ('/' :) . getConst (suffix @v))
