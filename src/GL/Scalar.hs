module GL.Scalar where

import Data.Proxy
import qualified Foreign.Storable as S
import Graphics.GL.Core41
import Graphics.GL.Types

class (Num n, S.Storable n) => Scalar n where
  glType :: Proxy n -> GLenum

instance Scalar Float where
  glType _ = GL_FLOAT

instance Scalar Double where
  glType _ = GL_DOUBLE
