module GL.Framebuffer where

import GL.Object
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Framebuffer = Framebuffer { unFramebuffer :: GLuint }

instance GLObject Framebuffer where characterize = (Framebuffer, glGenFramebuffers, glDeleteFramebuffers)
