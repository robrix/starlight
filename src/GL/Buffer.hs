{-# LANGUAGE TypeApplications #-}
module GL.Buffer
( Buffer(..)
) where

import Data.Coerce
import GL.Object
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Buffer n = Buffer { unBuffer :: GLuint }

instance Object (Buffer n) where
  construct = coerce
  gen = coerce (glGenBuffers @IO)
  delete = coerce (glDeleteBuffers @IO)
