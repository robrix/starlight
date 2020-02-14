module GL.Viewport
( min_
, max_
, viewport
, scissor
) where

import Control.Monad.IO.Class.Lift
import Data.Functor.Interval
import Graphics.GL.Core41
import Linear.V2

viewport :: (Integral a, Has (Lift IO) sig m) => Interval V2 a -> m ()
viewport i = runLiftIO (glViewport x y w h) where
  V2 x y = fromIntegral <$> inf  i
  V2 w h = fromIntegral <$> size i

scissor :: (Integral a, Has (Lift IO) sig m) => Interval V2 a -> m ()
scissor i = runLiftIO (glScissor x y w h) where
  V2 x y = fromIntegral <$> inf  i
  V2 w h = fromIntegral <$> size i
