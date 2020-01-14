module GL.Viewport
( min_
, max_
, viewport
, scissor
, Bounding(..)
) where

import Control.Monad.IO.Class.Lift
import Data.Functor.Interval
import Graphics.GL.Core41
import Linear.V2

viewport :: Has (Lift IO) sig m => Interval V2 Int -> m ()
viewport i = runLiftIO (glViewport x y w h) where
  V2 x y = fromIntegral <$> min' i
  V2 w h = fromIntegral <$> size i

scissor :: Has (Lift IO) sig m => Interval V2 Int -> m ()
scissor i = runLiftIO (glScissor x y w h) where
  V2 x y = fromIntegral <$> min' i
  V2 w h = fromIntegral <$> size i
