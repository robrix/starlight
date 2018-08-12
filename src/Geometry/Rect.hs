{-# LANGUAGE DeriveFunctor, RecordWildCards #-}
module Geometry.Rect where

import Lens.Micro
import Lens.Micro.Extras
import Linear.V2 as Linear
import Linear

data Rect a = Rect
  { rectMin :: {-# UNPACK #-} !(V2 a)
  , rectMax :: {-# UNPACK #-} !(V2 a)
  }
  deriving Functor

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


scaleRect :: Num a => a -> Rect a -> Rect a
scaleRect scale Rect{..} = Rect (rectMin ^* scale) (rectMax ^* scale)
