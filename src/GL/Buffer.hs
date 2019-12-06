{-# LANGUAGE GeneralizedNewtypeDeriving, TypeApplications #-}
module GL.Buffer
( Buffer(..)
) where

import Data.Coerce
import Foreign.Storable
import GL.Object
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Buffer n = Buffer { unBuffer :: GLuint }
  deriving (Storable)

instance Object (Buffer n) where
  gen = coerce (glGenBuffers @IO)
  delete = coerce (glDeleteBuffers @IO)
