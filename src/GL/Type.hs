{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module GL.Type
( Type(..)
) where

import           Data.Int
import           Data.Proxy
import qualified Foreign.Storable as S
import           Graphics.GL.Core41
import           Graphics.GL.Types
import           Linear.Affine
import           Linear.V1
import           Linear.V2
import           Linear.V3
import           Linear.V4

class S.Storable n => Type n where
  glType :: proxy n -> GLenum

  glDims :: proxy n -> GLint
  glDims _ = 1

instance Type Float where
  glType _ = GL_FLOAT

instance Type Double where
  glType _ = GL_DOUBLE

instance Type Int where
  glType _ = GL_INT

instance Type Int32 where
  glType _ = GL_INT

instance Type a => Type (V1 a) where
  glType _ = glType (Proxy @a)

  glDims _ = glDims (Proxy @a)

instance Type a => Type (V2 a) where
  glType _ = glType (Proxy @a)

  glDims _ = 2 * glDims (Proxy @a)

instance Type a => Type (V3 a) where
  glType _ = glType (Proxy @a)

  glDims _ = 3 * glDims (Proxy @a)

instance Type a => Type (V4 a) where
  glType _ = glType (Proxy @a)

  glDims _ = 4 * glDims (Proxy @a)

instance Type (f a) => Type (Point f a) where
  glType _ = glType (Proxy @(f a))

  glDims _ = glDims (Proxy @(f a))
