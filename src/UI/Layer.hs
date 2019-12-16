module UI.Layer
( Contents(..)
, drawLayer
) where

import Control.Monad.IO.Class.Lift
import Geometry.Rect
import GL.Framebuffer
import Graphics.GL.Core41
import Linear.V2
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

  s <- W.scale
  let Rect (V2 x y) (V2 w h) = fromIntegral <$> s *^ bounds
  glViewport x y w h
  glScissor x y w h

  case background of
    Just colour -> do
      setClearColour colour
      glClear GL_COLOR_BUFFER_BIT
    _ -> pure ()

  LiftIO draw
