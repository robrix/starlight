module GL.TextureUnit where

import GL.Error
import GL.Uniform
import Graphics.GL.Core41
import Graphics.GL.Types

newtype TextureUnit = TextureUnit { unTextureUnit :: GLint }

instance Uniform TextureUnit where
  uniform location textureUnit = checkingGLError $ glUniform1i location (unTextureUnit textureUnit)
