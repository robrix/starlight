{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GL.Buffer
( Buffer(..)
) where

import Control.Monad.IO.Class.Lift
import Data.Coerce
import Foreign.Storable
import GL.Error
import GL.Object
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Buffer n = Buffer { unBuffer :: GLuint }
  deriving (Storable)

instance Object (Buffer n) where
  gen n = glGenBuffers n . coerce
  delete n = glDeleteBuffers n . coerce

instance Bind (Buffer n) where
  nullObject = Buffer 0
  bindObject = checkingGLError . runLiftIO . glBindBuffer GL_ARRAY_BUFFER . unBuffer
