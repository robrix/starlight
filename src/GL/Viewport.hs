module GL.Viewport
( _min
, _max
, viewport
, scissor
, Bounding(..)
) where

import Control.Monad.IO.Class.Lift
import Data.Functor.Interval
import Graphics.GL.Core41
import Linear.V2

viewport :: Has (Lift IO) sig m => Interval V2 Int -> m ()
viewport r = runLiftIO (glViewport x1 y1 x2 y2) where
  Interval (V2 x1 y1) (V2 x2 y2) = fromIntegral <$> r

scissor :: Has (Lift IO) sig m => Interval V2 Int -> m ()
scissor r = runLiftIO (glScissor x1 y1 x2 y2) where
  Interval (V2 x1 y1) (V2 x2 y2) = fromIntegral <$> r
