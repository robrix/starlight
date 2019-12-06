{-# LANGUAGE TypeApplications #-}
module GL.Framebuffer
( Framebuffer(..)
, bindFramebuffer
) where

import Data.Coerce
import GL.Error
import GL.Object
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Framebuffer = Framebuffer { unFramebuffer :: GLuint }

instance Object Framebuffer where
  construct = coerce
  gen = coerce (glGenFramebuffers @IO)
  delete = coerce (glDeleteFramebuffers @IO)


bindFramebuffer :: Maybe Framebuffer -> IO ()
bindFramebuffer = checkingGLError . glBindFramebuffer GL_FRAMEBUFFER . maybe 0 unFramebuffer
