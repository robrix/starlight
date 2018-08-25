module GL.TextureUnit where

import GL.Uniform
import Graphics.GL.Core41
import Graphics.GL.Types

newtype TextureUnit = TextureUnit { unTextureUnit :: GLint }

instance Uniform TextureUnit where
  uniform location = glUniform1i location . unTextureUnit
