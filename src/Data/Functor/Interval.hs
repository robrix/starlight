{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeApplications #-}
module Data.Functor.Interval
( Interval(..)
, point
, size
, toUnit
, fromUnit
, wrap
, min_
, max_
, Bounding(..)
) where

import Control.Applicative (liftA2)
import Control.Lens
import Data.Fixed (mod')
import Data.Generics.Product.Fields
import GHC.Generics (Generic)

data Interval f a = Interval
  { min' :: !(f a)
  , max' :: !(f a)
  }
  deriving (Eq, Foldable, Functor, Generic, Show, Traversable)

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


point :: f a -> Interval f a
point fa = Interval fa fa

size :: Num (f a) => Interval f a -> f a
size (Interval min max) = max - min

toUnit, fromUnit :: Fractional (f a) => Interval f a -> f a -> f a
toUnit   i x = (x - min' i) / size i
fromUnit i x =  x * size i  + min' i


wrap :: Real (f a) => Interval f a -> f a -> f a
wrap i x = ((x + max' i) `mod'` size i) + min' i


min_ :: Lens' (Interval f a) (f a)
min_ = field @"min'"

max_ :: Lens' (Interval f a) (f a)
max_ = field @"max'"


newtype Bounding f a = Bounding { getBounding :: Interval f a }

instance (Applicative f, Ord a) => Semigroup (Bounding f a) where
  Bounding i1 <> Bounding i2 = Bounding (Interval (pure min) (pure max) <*> i1 <*> i2)
