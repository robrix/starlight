{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GL.TextureUnit
( TextureUnit(..)
, setActiveTexture
) where

import Control.Monad.IO.Class.Lift
import Foreign.Storable
import GL.Type as GL
import GL.Uniform
import Graphics.GL.Core41
import Graphics.GL.Types

newtype TextureUnit = TextureUnit { unTextureUnit :: GLint }
  deriving (Storable)

instance GL.Type TextureUnit where
  glType _ = GL_INT

instance Uniform TextureUnit where
  uniform loc = runLiftIO . glUniform1i loc . unTextureUnit


setActiveTexture :: Has (Lift IO) sig m => TextureUnit -> m ()
setActiveTexture (TextureUnit i) = runLiftIO $ glActiveTexture (fromIntegral (GL_TEXTURE0 + i))
