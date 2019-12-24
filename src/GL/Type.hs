module GL.Type
( Type(..)
) where

import qualified Foreign.Storable as S
import           Graphics.GL.Core41
import           Graphics.GL.Types

class S.Storable n => Type n where
  glType :: proxy n -> GLenum

instance Type Float where
  glType _ = GL_FLOAT

instance Type Double where
  glType _ = GL_DOUBLE
