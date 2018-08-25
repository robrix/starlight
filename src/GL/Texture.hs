module GL.Texture where

import GL.Error
import GL.Object
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Texture = Texture { unTexture :: GLuint }

instance Object Texture where characterize = (Texture, glGenTextures, glDeleteTextures)


data Target = Texture2D

bindTexture :: Target -> Texture -> IO ()
bindTexture target = checkingGLError . glBindTexture (toGLEnum target) . unTexture
  where toGLEnum Texture2D = GL_TEXTURE_2D
