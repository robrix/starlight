module GL.Framebuffer where

import GL.Error
import GL.Object
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Framebuffer = Framebuffer { unFramebuffer :: GLuint }

instance Object Framebuffer where characterize = (Framebuffer, glGenFramebuffers, glDeleteFramebuffers)


bindFramebuffer :: Framebuffer -> IO ()
bindFramebuffer = checkingGLError . glBindFramebuffer GL_FRAMEBUFFER . unFramebuffer
