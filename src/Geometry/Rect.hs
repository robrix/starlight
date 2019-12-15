{-# LANGUAGE DeriveFunctor #-}
module Geometry.Rect
( Rect(..)
, _rectMin
, _rectMax
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

_rectMin :: Lens' (Rect a) (V2 a)
_rectMin = lens rectMin (\ r v -> r { rectMin = v })

_rectMax :: Lens' (Rect a) (V2 a)
_rectMax = lens rectMax (\ r v -> r { rectMax = v })


minX :: Rect a -> a
minX = view _x . rectMin

minY :: Rect a -> a
minY = view _y . rectMin

maxX :: Rect a -> a
maxX = view _x . rectMax

maxY :: Rect a -> a
maxY = view _y . rectMax


transformRect :: Num a => M33 a -> Rect a -> Rect a
transformRect m (Rect v1 v2) = Rect ((m !* ext v1) ^. _xy) ((m !* ext v2) ^. _xy)
  where ext (V2 x y) = V3 x y 1


newtype Union a = Union { getUnion :: Rect a }

instance Ord a => Semigroup (Union a) where
  Union (Rect min1 max1) <> Union (Rect min2 max2) = Union (Rect (min <$> min1 <*> min2) (max <$> max1 <*> max2))
