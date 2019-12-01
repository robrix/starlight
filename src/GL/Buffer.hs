module GL.Buffer
( Buffer(..)
) where

import GL.Object
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Buffer n = Buffer { unBuffer :: GLuint }

instance Object (Buffer n) where characterize = (Buffer, glGenBuffers, glDeleteBuffers)
