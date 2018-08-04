module GL.Scalar where

import Data.Proxy
import qualified Foreign.Storable as S
import Graphics.GL.Core41
import Graphics.GL.Types

class (Num n, S.Storable n) => GLScalar n where
  glType :: Proxy n -> GLenum

instance GLScalar Float where
  glType _ = GL_FLOAT

instance GLScalar Double where
  glType _ = GL_DOUBLE
