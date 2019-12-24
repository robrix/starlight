{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module GL.Type
( Type(..)
) where

import           Data.Proxy
import qualified Foreign.Storable as S
import           Graphics.GL.Core41
import           Graphics.GL.Types
import           Linear.V2
import           Linear.V3

class S.Storable n => Type n where
  glType :: proxy n -> GLenum

instance Type Float where
  glType _ = GL_FLOAT

instance Type Double where
  glType _ = GL_DOUBLE

instance Type Int where
  glType _ = GL_INT

instance Type a => Type (V2 a) where
  glType _ = glType (Proxy @a)

instance Type a => Type (V3 a) where
  glType _ = glType (Proxy @a)
