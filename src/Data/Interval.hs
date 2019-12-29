{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms #-}
module Data.Interval
( Interval
, pattern Interval
, min_
, max_
, size
, toUnit
, fromUnit
, wrap
) where

import Data.Fixed (mod')
import Data.Functor.I
import qualified Data.Functor.Interval as F

type Interval = F.Interval I

pattern Interval :: a -> a -> Interval a
pattern Interval a b = F.Interval (I a) (I b)

{-# COMPLETE Interval #-}

min_, max_ :: Interval a -> a

min_ (Interval a _) = a
max_ (Interval _ b) = b


size :: Num a => Interval a -> a
size (Interval min max) = max - min

toUnit, fromUnit :: Fractional a => Interval a -> a -> a
toUnit   i x = (x - min_ i) / size i
fromUnit i x =  x * size i  + min_ i


wrap :: Real a => Interval a -> a -> a
wrap i x = ((x + max_ i) `mod'` size i) + min_ i
