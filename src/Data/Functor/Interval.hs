{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Data.Functor.Interval
( Interval(..)
, (...)
, point
, pointwise
, size
, toUnit
, fromUnit
, range
, wrap
, inf_
, sup_
, imap
, member
, isSubintervalOf
, isProperSubintervalOf
, uniformI
, Union(..)
, union
, Intersection(..)
, intersection
) where

import           Control.Applicative (liftA2)
import           Control.Effect.Random
import           Control.Lens hiding (imap, (...))
import           Control.Monad (join)
import           Control.Monad.Trans.Class
import           Data.Coerce (coerce)
import           Data.Fixed (mod')
import           Data.Functor.I
import           Data.Generics.Product.Fields
import           GHC.Generics (Generic)
import qualified System.Random as R

data Interval f a = Interval
  { inf :: !(f a)
  , sup :: !(f a)
  }
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

instance Applicative f => Applicative (Interval f) where
  pure = point . pure
  f <*> a = Interval (inf f <*> inf a) (sup f <*> sup a)

instance Monad f => Monad (Interval f) where
  m >>= f = Interval (inf m >>= inf . f) (sup m >>= sup . f)

instance MonadTrans Interval where
  lift = point

instance (Applicative f, Num a) => Num (Interval f a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  negate = fmap negate
  {-# INLINE negate #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

instance (Applicative f, Fractional a) => Fractional (Interval f a) where
  recip = fmap recip
  {-# INLINE recip #-}
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance (Applicative f, Floating a) => Floating (Interval f a) where
  pi = pure pi
  {-# INLINE pi #-}
  exp = fmap exp
  {-# INLINE exp #-}
  sqrt = fmap sqrt
  {-# INLINE sqrt #-}
  log = fmap log
  {-# INLINE log #-}
  (**) = liftA2 (**)
  {-# INLINE (**) #-}
  logBase = liftA2 logBase
  {-# INLINE logBase #-}
  sin = fmap sin
  {-# INLINE sin #-}
  tan = fmap tan
  {-# INLINE tan #-}
  cos = fmap cos
  {-# INLINE cos #-}
  asin = fmap asin
  {-# INLINE asin #-}
  atan = fmap atan
  {-# INLINE atan #-}
  acos = fmap acos
  {-# INLINE acos #-}
  sinh = fmap sinh
  {-# INLINE sinh #-}
  tanh = fmap tanh
  {-# INLINE tanh #-}
  cosh = fmap cosh
  {-# INLINE cosh #-}
  asinh = fmap asinh
  {-# INLINE asinh #-}
  atanh = fmap atanh
  {-# INLINE atanh #-}
  acosh = fmap acosh
  {-# INLINE acosh #-}


(...) :: Applicative f => a -> a -> Interval f a
inf...sup = Interval (pure inf) (pure sup)

infix 3 ...

point :: f a -> Interval f a
point = join Interval

pointwise :: Applicative f => (Interval I a -> b) -> Interval f a -> f b
pointwise f i = fmap f . (...) <$> inf i <*> sup i

size :: (Applicative f, Num a) => Interval f a -> f a
size = liftA2 (-) . sup <*> inf

toUnit, fromUnit :: (Applicative f, Fractional a) => Interval f a -> f a -> f a
toUnit   i x = pointwise (\ i x -> getI ((I x - inf  i) / size i)) i <*> x
fromUnit i x = pointwise (\ i x -> getI  (I x * size i  + inf  i)) i <*> x


range :: Enum (f a) => Interval f a -> [f a]
range = enumFromTo . inf <*> sup


wrap :: (Applicative f, Real a) => Interval f a -> f a -> f a
wrap i x = pointwise (\ i x -> getI (((I x + sup i) `mod'` size i) + inf i)) i <*> x


inf_ :: Lens' (Interval f a) (f a)
inf_ = field @"inf"

sup_ :: Lens' (Interval f a) (f a)
sup_ = field @"sup"


imap :: (f a -> g b) -> Interval f a -> Interval g b
imap f = Interval <$> f . inf <*> f . sup


member :: (Applicative f, Foldable f, Ord a) => f a -> Interval f a -> Bool
member = isSubintervalOf . point


isSubintervalOf :: (Applicative f, Foldable f, Ord a) => Interval f a -> Interval f a -> Bool
isSubintervalOf a b = and ((>=) <$> inf a <*> inf b) && and ((<=) <$> sup a <*> sup b)

isProperSubintervalOf :: (Applicative f, Foldable f, Ord a) => Interval f a -> Interval f a -> Bool
isProperSubintervalOf a b = and ((>) <$> inf a <*> inf b) && and ((<) <$> sup a <*> sup b)


uniformI :: (R.Random a, Applicative f, Traversable f, Has Random sig m) => Interval f a -> m (f a)
uniformI i = traverse uniformR ((,) <$> inf i <*> sup i)


newtype Union f a = Union { getUnion :: Interval f a }
  deriving (Applicative, Eq, Foldable, Functor, Monad, Ord, Show, Traversable)

instance (Applicative f, Ord a) => Semigroup (Union f a) where
  Union i1 <> Union i2 = Union ((min...max) <*> i1 <*> i2)

union :: forall f a . (Applicative f, Ord a) => Interval f a -> Interval f a -> Interval f a
union = coerce ((<>) :: Union f a -> Union f a -> Union f a)


newtype Intersection f a = Intersection { getIntersection :: Interval f a }
  deriving (Applicative, Eq, Foldable, Functor, Monad, Ord, Show, Traversable)

instance (Applicative f, Ord a) => Semigroup (Intersection f a) where
  Intersection i1 <> Intersection i2 = Intersection ((max...min) <*> i1 <*> i2)

intersection :: forall f a . (Applicative f, Ord a) => Interval f a -> Interval f a -> Interval f a
intersection = coerce ((<>) :: Intersection f a -> Intersection f a -> Intersection f a)
