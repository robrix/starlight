{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Data.Functor.Interval
( Interval(..)
, interval
, point
, size
, toUnit
, fromUnit
, range
, wrap
, min_
, max_
, imap
, uniformI
, Union(..)
, Intersection(..)
) where

import           Control.Applicative (liftA2)
import           Control.Effect.Random
import           Control.Lens hiding (imap)
import           Data.Fixed (mod')
import           Data.Generics.Product.Fields
import           GHC.Generics (Generic)
import qualified System.Random as R

data Interval f a = Interval
  { min' :: !(f a)
  , max' :: !(f a)
  }
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

instance Applicative f => Applicative (Interval f) where
  pure a = Interval (pure a) (pure a)
  Interval f1 f2 <*> Interval a1 a2 = Interval (f1 <*> a1) (f2 <*> a2)

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


interval :: Applicative f => a -> a -> Interval f a
interval mn mx = Interval (pure mn) (pure mx)

point :: f a -> Interval f a
point fa = Interval fa fa

size :: Num (f a) => Interval f a -> f a
size (Interval min max) = max - min

toUnit, fromUnit :: Fractional (f a) => Interval f a -> f a -> f a
toUnit   i x = (x - min' i) / size i
fromUnit i x =  x * size i  + min' i


range :: Enum (f a) => Interval f a -> [f a]
range = enumFromTo . min' <*> max'


wrap :: Real (f a) => Interval f a -> f a -> f a
wrap i x = ((x + max' i) `mod'` size i) + min' i


min_ :: Lens' (Interval f a) (f a)
min_ = field @"min'"

max_ :: Lens' (Interval f a) (f a)
max_ = field @"max'"


imap :: (f a -> g b) -> Interval f a -> Interval g b
imap f = Interval <$>  f . min' <*> f . max'


uniformI :: (R.Random a, Applicative f, Traversable f, Has Random sig m) => Interval f a -> m (f a)
uniformI (Interval mn mx) = traverse uniformR ((,) <$> mn <*> mx)


newtype Union f a = Union { getUnion :: Interval f a }
  deriving (Applicative, Eq, Foldable, Functor, Ord, Show, Traversable)

instance (Applicative f, Ord a) => Semigroup (Union f a) where
  Union i1 <> Union i2 = Union (interval min max <*> i1 <*> i2)


newtype Intersection f a = Intersection { getIntersection :: Interval f a }
  deriving (Applicative, Eq, Foldable, Functor, Ord, Show, Traversable)

instance (Applicative f, Ord a) => Semigroup (Intersection f a) where
  Intersection i1 <> Intersection i2 = Intersection (interval max min <*> i1 <*> i2)
