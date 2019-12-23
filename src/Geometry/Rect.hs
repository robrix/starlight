{-# LANGUAGE DeriveFunctor #-}
module Geometry.Rect
( Rect(..)
, _min
, _max
, outsetToIntegralCoords
, transformRect
, viewport
, scissor
, Union(..)
) where

import Control.Monad.IO.Class.Lift
import Graphics.GL.Core41
import Lens.Micro
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


outsetToIntegralCoords :: RealFrac a => Rect a -> Rect Int
outsetToIntegralCoords (Rect min max) = Rect (floor <$> min) (ceiling <$> max)


transformRect :: Num a => M33 a -> Rect a -> Rect a
transformRect m (Rect v1 v2) = Rect ((m !* ext v1) ^. _xy) ((m !* ext v2) ^. _xy)
  where ext (V2 x y) = V3 x y 1


viewport :: Has (Lift IO) sig m => Rect Int -> m ()
viewport r = runLiftIO (glViewport x1 y1 x2 y2) where
  Rect (V2 x1 y1) (V2 x2 y2) = fromIntegral <$> r

scissor :: Has (Lift IO) sig m => Rect Int -> m ()
scissor r = runLiftIO (glScissor x1 y1 x2 y2) where
  Rect (V2 x1 y1) (V2 x2 y2) = fromIntegral <$> r


newtype Union a = Union { getUnion :: Rect a }

instance Ord a => Semigroup (Union a) where
  Union (Rect min1 max1) <> Union (Rect min2 max2) = Union (Rect (min <$> min1 <*> min2) (max <$> max1 <*> max2))
