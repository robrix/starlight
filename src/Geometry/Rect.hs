module Geometry.Rect where

import Lens.Micro
import Lens.Micro.Extras
import Linear.V2 as Linear

data Rect a = Rect
  { rectMin :: {-# UNPACK #-} !(V2 a)
  , rectMax :: {-# UNPACK #-} !(V2 a)
  }

_rectMin :: Lens' (Rect a) (V2 a)
_rectMin = lens rectMin (\ r v -> r { rectMin = v })

_rectMax :: Lens' (Rect a) (V2 a)
_rectMax = lens rectMax (\ r v -> r { rectMax = v })
