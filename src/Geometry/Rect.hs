{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
module Geometry.Rect
( Rect
, pattern Rect
, _min
, _max
, viewport
, scissor
, Bounding(..)
) where

import Control.Monad.IO.Class.Lift
import Data.Functor.Interval
import Graphics.GL.Core41
import Linear.V2

type Rect = Interval V2

pattern Rect :: V2 a -> V2 a -> Rect a
pattern Rect a b = Interval a b


viewport :: Has (Lift IO) sig m => Rect Int -> m ()
viewport r = runLiftIO (glViewport x1 y1 x2 y2) where
  Interval (V2 x1 y1) (V2 x2 y2) = fromIntegral <$> r

scissor :: Has (Lift IO) sig m => Rect Int -> m ()
scissor r = runLiftIO (glScissor x1 y1 x2 y2) where
  Interval (V2 x1 y1) (V2 x2 y2) = fromIntegral <$> r
