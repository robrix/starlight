{-# LANGUAGE FlexibleInstances #-}
module GL.Uniform where

import Data.Foldable (toList)
import qualified Foreign.C.String as C
import qualified Foreign.Marshal.Array as A
import Foreign.Ptr
import GHC.Stack
import GL.Error
import GL.Program
import GL.TextureUnit
import Graphics.GL.Core41
import Graphics.GL.Types
import Linear.Matrix as Linear
import Linear.V4 as Linear

newtype Var t = Var { varName :: String }

class Uniform t where
  glUniform :: HasCallStack => GLint -> t -> IO ()

setUniformValue :: (Uniform t, HasCallStack) => Program -> Var t -> t -> IO ()
setUniformValue program var v = do
  location <- checkingGLError $ C.withCString (varName var) (glGetUniformLocation (unProgram program))
  glUniform location v

instance Uniform (Linear.V4 Float) where
  glUniform location (Linear.V4 x y z w) = checkingGLError $ glUniform4f location x y z w

instance Uniform (Linear.V4 Double) where
  glUniform location (Linear.V4 x y z w) = checkingGLError $ glUniform4d location x y z w

instance Uniform (Linear.M44 Float) where
  glUniform location matrix = checkingGLError $ A.withArray (toList (Linear.transpose matrix) >>= toList) (glUniformMatrix4fv location 1 GL_FALSE . castPtr)

instance Uniform (Linear.M33 Float) where
  glUniform location matrix = checkingGLError $ A.withArray (toList (Linear.transpose matrix) >>= toList) (glUniformMatrix3fv location 1 GL_FALSE . castPtr)

instance Uniform TextureUnit where
  glUniform location textureUnit = checkingGLError $ glUniform1i location (unTextureUnit textureUnit)
