module UI.Layer
( Layer(..)
, Contents(..)
, drawLayer
) where

import Control.Monad.IO.Class.Lift
import Data.Int (Int32)
import Geometry.Rect
import GL.Framebuffer
import Graphics.GL.Core41
import Linear.V2
import UI.Colour

data Layer = Layer
  { framebuffer :: Maybe Framebuffer
  , background  :: Colour Float
  , bounds      :: Rect Int32
  , draw        :: IO ()
  }

data Contents
  = Colour (Colour Float)
  | Composite [Contents]

drawLayer :: Has (Lift IO) sig m => Layer -> m ()
drawLayer layer = runLifting $ do
  bindFramebuffer (framebuffer layer)

  let Rect (V2 x y) (V2 w h) = (2 *) <$> bounds layer
  glViewport x y w h
  glScissor x y w h

  setClearColour (background layer)
  glClear GL_COLOR_BUFFER_BIT

  sendM (draw layer)
