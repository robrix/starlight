module GL.Texture where

import GL.Object
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Texture = Texture { unTexture :: GLuint }

instance Object Texture where characterize = (Texture, glGenTextures, glDeleteTextures)
