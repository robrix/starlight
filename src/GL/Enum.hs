module GL.Enum
( Enum(..)
) where

import Graphics.GL.Types
import Prelude hiding (Enum)

class Enum t where
  glEnum :: t -> GLenum
