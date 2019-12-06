{-# LANGUAGE TypeApplications #-}
module GL.Texture
( Texture(..)
, Target(..)
, targetToGLEnum
, bindTexture
, Filter(..)
, filterToGLEnum
, setMagFilter
, setMinFilter
) where

import Data.Coerce
import GL.Error
import GL.Object
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Texture = Texture { unTexture :: GLuint }

instance Object Texture where
  construct = coerce
  gen = coerce (glGenTextures @IO)
  delete = coerce (glDeleteTextures @IO)


data Target = Texture2D

targetToGLEnum :: Target -> GLenum
targetToGLEnum Texture2D = GL_TEXTURE_2D


bindTexture :: Target -> Maybe Texture -> IO ()
bindTexture target = checkingGLError . glBindTexture (targetToGLEnum target) . maybe 0 unTexture


data Filter = Nearest | Linear

filterToGLEnum :: Filter -> GLenum
filterToGLEnum Nearest = GL_NEAREST
filterToGLEnum Linear = GL_LINEAR

setMagFilter :: Target -> Filter -> IO ()
setMagFilter target = checkingGLError . glTexParameteri (targetToGLEnum target) GL_TEXTURE_MAG_FILTER . fromIntegral . filterToGLEnum

setMinFilter :: Target -> Filter -> IO ()
setMinFilter target = checkingGLError . glTexParameteri (targetToGLEnum target) GL_TEXTURE_MIN_FILTER . fromIntegral . filterToGLEnum
