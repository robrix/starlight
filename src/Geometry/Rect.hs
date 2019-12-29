{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
module Geometry.Rect
( Rect
, pattern Rect
, rectMin
, rectMax
, _min
, _max
, outsetToIntegralCoords
, transformRect
, viewport
, scissor
, Bounding(..)
) where

import Control.Monad.IO.Class.Lift
import Data.Functor.Interval
import Graphics.GL.Core41
import Lens.Micro
import Linear.Exts
import Linear.Matrix
import Linear.V2

type Rect = Interval V2

pattern Rect :: V2 a -> V2 a -> Rect a
pattern Rect a b = Interval a b

rectMin :: Rect a -> V2 a
rectMin = min_

rectMax :: Rect a -> V2 a
rectMax = max_


outsetToIntegralCoords :: RealFrac a => Rect a -> Rect Int
outsetToIntegralCoords (Interval min max) = Interval (floor <$> min) (ceiling <$> max)


transformRect :: Num a => M33 a -> Rect a -> Rect a
transformRect m (Interval v1 v2) = Interval ((m !* ext v1 1) ^. _xy) ((m !* ext v2 1) ^. _xy)


viewport :: Has (Lift IO) sig m => Rect Int -> m ()
viewport r = runLiftIO (glViewport x1 y1 x2 y2) where
  Interval (V2 x1 y1) (V2 x2 y2) = fromIntegral <$> r

scissor :: Has (Lift IO) sig m => Rect Int -> m ()
scissor r = runLiftIO (glScissor x1 y1 x2 y2) where
  Interval (V2 x1 y1) (V2 x2 y2) = fromIntegral <$> r
