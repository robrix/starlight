{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Unit.Algebra
(  -- * Algebra
  Mul(..)
, Recip(..)
, Div(..)
  -- * Combinators
, (:/:)(..)
, (:*:)(..)
) where

import Control.Applicative (liftA2)
import Data.Functor.Const
import Foreign.Storable
import GHC.Generics ((:.:)(..))
import GL.Type as GL
import GL.Uniform
import Linear.Conjugate
import Linear.Epsilon
import Linear.Metric
import Linear.Vector
import Unit

-- * Algebra

class (Functor u, Functor v, Functor w) => Mul u v w | u w -> v where
  (.*.) :: Fractional a => u a -> v a -> w a

infixl 7 .*.

class (Functor u, Functor v, Functor w) => Div u v w | u w -> v where
  (./.) :: Fractional a => u a -> v a -> w a

infixl 7 ./.

instance {-# OVERLAPPABLE #-} (Functor u, Functor v) => Mul u v (u :*: v) where
  u .*. v = Prd ((*^ v) <$> u)

instance {-# OVERLAPPABLE #-} (Functor u, Functor v) => Div u v (u :/: v) where
  u ./. v = Per ((u ^/) <$> v)

instance {-# OVERLAPPABLE #-} (Functor u, Unit v) => Mul (u :/: v) v u where
  -- FIXME: there has got to be a better way to do this than assuming Unit & essentially that u & v each contain exactly 1 thing
  Per vu .*. v = prj vu ^* prj v

instance {-# OVERLAPPABLE #-} (Unit u, Functor v) => Div (u :*: v) u v where
  -- FIXME: there has got to be a better way to do this than assuming Unit & essentially that u & v each contain exactly 1 thing
  Prd uv ./. u = prj uv ^/ prj u

instance {-# OVERLAPPABLE #-} (Functor u, Unit v) => Div (u :*: v) v u where
  -- FIXME: there has got to be a better way to do this than assuming Unit & essentially that u & v each contain exactly 1 thing
  Prd uv ./. v = (prj <$> uv) ^/ prj v


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


newtype Recip u a = Recip { getRecip :: u a }
  deriving (Additive, Applicative, Conjugate, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Metric, Monad, Num, Ord, Real, RealFloat, RealFrac, Show, Storable, Traversable, GL.Type, Uniform)

instance Unit u => Unit (Recip u) where
  prj = prj . getRecip
  factor = Const (1/getConst (factor @u))
  suffix = Const (getConst (suffix @u) . ('⁻' :) . ('¹' :))


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
