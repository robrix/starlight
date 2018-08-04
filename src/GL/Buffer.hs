module GL.Buffer where

import GL.Object
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Buffer n = Buffer { unBuffer :: GLuint }

instance GLObject (Buffer n) where characterize = (Buffer, glGenBuffers, glDeleteBuffers)
