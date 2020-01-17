{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module GL.Type
( Type(..)
) where

import           Data.Int
import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Functor.K
import qualified Foreign.Storable as S
import           Graphics.GL.Core41
import           Graphics.GL.Types
import           Linear.Affine
import           Linear.V1
import           Linear.V2
import           Linear.V3
import           Linear.V4

class S.Storable n => Type n where
  glType :: K GLenum n

  glDims :: K GLint n
  glDims = 1

instance Type Bool where
  glType = GL_BOOL

instance Type Float where
  glType = GL_FLOAT

instance Type Double where
  glType = GL_DOUBLE

instance Type Int where
  glType = GL_INT

instance Type Int32 where
  glType = GL_INT

instance Type a => Type (V1 a) where
  glType = pure <$> glType

  glDims = pure <$> glDims

instance Type a => Type (V2 a) where
  glType = pure <$> glType

  glDims = pure <$> 2 * glDims

instance Type a => Type (V3 a) where
  glType = pure <$> glType

  glDims = pure <$> 3 * glDims

instance Type a => Type (V4 a) where
  glType = pure <$> glType

  glDims = pure <$> 4 * glDims

instance Type (f a) => Type (Point f a) where
  glType = P <$> glType

  glDims = P <$> glDims

deriving instance Type a => Type (Const a b)
deriving instance Type a => Type (Identity a)
deriving instance Type a => Type (K a b)
