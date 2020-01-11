{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

class (Functor u, Functor v, Functor w) => Mul u v w where
  (.*.) :: Fractional a => u a -> v a -> w a

infixl 7 .*.

class (Functor u, Functor v, Functor w) => Div u v w where
  (./.) :: Fractional a => u a -> v a -> w a

infixl 7 ./.

instance {-# OVERLAPPABLE #-} Functor u => Mul Identity u u where
  Identity a .*. b = a *^ b

instance {-# OVERLAPPABLE #-} Functor u => Div u Identity u where
  n ./. Identity d = n ^/ d

instance {-# OVERLAPPABLE #-} (Functor u, Functor v) => Mul u v (u :*: v) where
  u .*. v = Prd ((*^ v) <$> u)

instance {-# OVERLAPPABLE #-} (Functor u, Functor v) => Div u v (u :/: v) where
  u ./. v = Per ((u ^/) <$> v)


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


newtype (u :/: v) a = Per { getPer :: v (u a) }
  deriving (Eq, Foldable, Floating, Fractional, Functor, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)
  deriving Applicative via v :.: u

infixl 7 :/:

instance (Applicative v, Additive u) => Additive (u :/: v) where
  zero = Per (pure zero)
  liftU2 f (Per a) (Per b) = Per (liftA2 (liftU2 f) a b)
  liftI2 f (Per a) (Per b) = Per (liftA2 (liftI2 f) a b)

instance (Applicative v, Foldable v, Additive u, Foldable u) => Metric (u :/: v)

instance (Unit u, Unit v) => Unit (u :/: v) where
  prj = prj . prj . getPer
  factor = Const (getConst (factor @v) / getConst (factor @u))
  suffix = Const (getConst (suffix @u) . ('/' :) . getConst (suffix @v))
