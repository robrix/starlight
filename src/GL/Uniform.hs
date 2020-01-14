{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module GL.Uniform
( Uniform(..)
, Scalar(..)
) where

import           Control.Monad.IO.Class.Lift
import           Data.Int
import qualified Foreign.Marshal.Utils.Lift as A
import           Foreign.Ptr
import           GHC.Stack
import qualified GL.Type as GL
import           Graphics.GL.Core41
import           Graphics.GL.Types
import           Linear.Affine
import           Linear.Matrix
import           Linear.V2
import           Linear.V3
import           Linear.V4

class GL.Type t => Uniform t where
  glslType :: String
  uniform :: (HasCallStack, Has (Lift IO) sig m) => GLuint -> GLint -> t -> m ()

instance Uniform Bool where
  glslType = "bool"
  uniform prog loc = runLiftIO . glProgramUniform1i prog loc . fromIntegral . fromEnum

instance Uniform Int where
  glslType = "int"
  uniform prog loc = runLiftIO . glProgramUniform1i prog loc . fromIntegral

instance Uniform Int32 where
  glslType = "int"
  uniform prog loc = runLiftIO . glProgramUniform1i prog loc

instance Uniform Float where
  glslType = "float"
  uniform prog loc = runLiftIO . glProgramUniform1f prog loc

instance Uniform Double where
  glslType = "double"
  uniform prog loc = runLiftIO . glProgramUniform1d prog loc

instance Uniform (V2 Float) where
  glslType = "vec2"
  uniform prog loc vec = A.with vec (runLiftIO . glProgramUniform2fv prog loc 1 . castPtr)

instance Uniform (V2 Double) where
  glslType = "dvec2"
  uniform prog loc vec = A.with vec (runLiftIO . glProgramUniform2dv prog loc 1 . castPtr)

instance Uniform (V3 Float) where
  glslType = "vec3"
  uniform prog loc vec = A.with vec (runLiftIO . glProgramUniform3fv prog loc 1 . castPtr)

instance Uniform (V3 Double) where
  glslType = "dvec3"
  uniform prog loc vec = A.with vec (runLiftIO . glProgramUniform3dv prog loc 1 . castPtr)

instance Uniform (V4 Float) where
  glslType = "vec4"
  uniform prog loc vec = A.with vec (runLiftIO . glProgramUniform4fv prog loc 1 . castPtr)

instance Uniform (V4 Double) where
  glslType = "dvec4"
  uniform prog loc vec = A.with vec (runLiftIO . glProgramUniform4dv prog loc 1 . castPtr)

instance Uniform (M22 Float) where
  glslType = "mat2"
  uniform prog loc matrix = A.with matrix (runLiftIO . glProgramUniformMatrix2fv prog loc 1 GL_TRUE . castPtr)

instance Uniform (M22 Double) where
  glslType = "dmat2"
  uniform prog loc matrix = A.with matrix (runLiftIO . glProgramUniformMatrix2dv prog loc 1 GL_TRUE . castPtr)

instance Uniform (M23 Float) where
  glslType = "mat2x3"
  uniform prog loc matrix = A.with matrix (runLiftIO . glProgramUniformMatrix2x3fv prog loc 1 GL_TRUE . castPtr)

instance Uniform (M23 Double) where
  glslType = "dmat2x3"
  uniform prog loc matrix = A.with matrix (runLiftIO . glProgramUniformMatrix2x3dv prog loc 1 GL_TRUE . castPtr)

instance Uniform (M33 Float) where
  glslType = "mat3"
  uniform prog loc matrix = A.with matrix (runLiftIO . glProgramUniformMatrix3fv prog loc 1 GL_TRUE . castPtr)

instance Uniform (M33 Double) where
  glslType = "dmat3"
  uniform prog loc matrix = A.with matrix (runLiftIO . glProgramUniformMatrix3dv prog loc 1 GL_TRUE . castPtr)

instance Uniform (M34 Float) where
  glslType = "mat3x4"
  uniform prog loc matrix = A.with matrix (runLiftIO . glProgramUniformMatrix3x4fv prog loc 1 GL_TRUE . castPtr)

instance Uniform (M34 Double) where
  glslType = "dmat3x4"
  uniform prog loc matrix = A.with matrix (runLiftIO . glProgramUniformMatrix3x4dv prog loc 1 GL_TRUE . castPtr)

instance Uniform (M44 Float) where
  glslType = "mat4"
  uniform prog loc matrix = A.with matrix (runLiftIO . glProgramUniformMatrix4fv prog loc 1 GL_TRUE . castPtr)

instance Uniform (M44 Double) where
  glslType = "dmat4"
  uniform prog loc matrix = A.with matrix (runLiftIO . glProgramUniformMatrix4dv prog loc 1 GL_TRUE . castPtr)

deriving instance Uniform (f a) => Uniform (Point f a)


class GL.Type t => Scalar t where
  glslTypeFor :: Int -> String

  uniform1 :: GLuint -> GLint -> GLsizei -> Ptr t -> IO ()
  uniform2 :: GLuint -> GLint -> GLsizei -> Ptr t -> IO ()
  uniform3 :: GLuint -> GLint -> GLsizei -> Ptr t -> IO ()
  uniform4 :: GLuint -> GLint -> GLsizei -> Ptr t -> IO ()

instance Scalar Float where
  glslTypeFor n = "vec" <> show n
  uniform1 = glProgramUniform1fv
  uniform2 = glProgramUniform2fv
  uniform3 = glProgramUniform3fv
  uniform4 = glProgramUniform4fv

instance Scalar Double where
  glslTypeFor n = "dvec" <> show n
  uniform1 = glProgramUniform1dv
  uniform2 = glProgramUniform2dv
  uniform3 = glProgramUniform3dv
  uniform4 = glProgramUniform4dv
