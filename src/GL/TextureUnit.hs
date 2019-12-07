module GL.TextureUnit
( TextureUnit(..)
, setActiveTexture
) where

import Control.Monad.IO.Class.Lift
import GL.Uniform
import Graphics.GL.Core41
import Graphics.GL.Types

newtype TextureUnit = TextureUnit { unTextureUnit :: GLint }

instance Uniform TextureUnit where
  uniform location = glUniform1i location . unTextureUnit


setActiveTexture :: Has (Lift IO) sig m => TextureUnit -> m ()
setActiveTexture (TextureUnit i) = runLifting $ glActiveTexture (fromIntegral (GL_TEXTURE0 + i))
