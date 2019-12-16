module UI.Layer
( Contents(..)
, drawLayer
) where

import Control.Monad.IO.Class.Lift
import Geometry.Rect
import GL.Framebuffer
import Graphics.GL.Core41
import Linear.Vector
import UI.Colour
import qualified UI.Effect.Window as W

data Contents
  = Colour (Colour Float)
  | Composite [Contents]

drawLayer
  :: (Has (Lift IO) sig m, Has W.Window sig m)
  => Maybe Framebuffer
  -> Maybe (Colour Float)
  -> Rect Int
  -> m a
  -> m a
drawLayer framebuffer background bounds draw = runLiftIO $ do
  bind framebuffer

  scale <- W.scale
  viewport $ scale *^ bounds
  scissor  $ scale *^ bounds

  case background of
    Just colour -> do
      setClearColour colour
      glClear GL_COLOR_BUFFER_BIT
    _ -> pure ()

  LiftIO draw
