{-# LANGUAGE DeriveFunctor #-}
module Geometry.Rect
( Rect(..)
, _min
, _max
, minX
, minY
, maxX
, maxY
, transformRect
, Union(..)
) where

import Lens.Micro
import Lens.Micro.Extras
import Linear.Matrix
import Linear.V2
import Linear.V3

data Rect a = Rect
  { rectMin :: {-# UNPACK #-} !(V2 a)
  , rectMax :: {-# UNPACK #-} !(V2 a)
  }
  deriving (Eq, Functor, Show)

_min :: Lens' (Rect a) (V2 a)
_min = lens rectMin (\ r v -> r { rectMin = v })

_max :: Lens' (Rect a) (V2 a)
_max = lens rectMax (\ r v -> r { rectMax = v })


minX :: Rect a -> a
minX = view (_min . _x)

minY :: Rect a -> a
minY = view (_min . _y)

maxX :: Rect a -> a
maxX = view (_max . _x)

maxY :: Rect a -> a
maxY = view (_max . _y)


transformRect :: Num a => M33 a -> Rect a -> Rect a
transformRect m (Rect v1 v2) = Rect ((m !* ext v1) ^. _xy) ((m !* ext v2) ^. _xy)
  where ext (V2 x y) = V3 x y 1


newtype Union a = Union { getUnion :: Rect a }

instance Ord a => Semigroup (Union a) where
  Union (Rect min1 max1) <> Union (Rect min2 max2) = Union (Rect (min <$> min1 <*> min2) (max <$> max1 <*> max2))
