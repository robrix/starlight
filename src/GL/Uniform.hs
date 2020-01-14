{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
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

instance {-# OVERLAPPABLE #-} Scalar t => Uniform (V2 t) where
  glslType = glslTypeFor @t 2
  uniform prog loc vec = A.with vec (sendM . uniformFor @t 2 prog loc 1 . castPtr)

instance {-# OVERLAPPABLE #-} Scalar t => Uniform (V3 t) where
  glslType = glslTypeFor @t 3
  uniform prog loc vec = A.with vec (sendM . uniformFor @t 3 prog loc 1 . castPtr)

instance {-# OVERLAPPABLE #-} Scalar t => Uniform (V4 t) where
  glslType = glslTypeFor @t 4
  uniform prog loc vec = A.with vec (sendM . uniformFor @t 4 prog loc 1 . castPtr)

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

  uniformFor :: Int -> GLuint -> GLint -> GLsizei -> Ptr t -> IO ()

instance Scalar Float where
  glslTypeFor 1 = "float"
  glslTypeFor n = "vec" <> show n
  {-# INLINE glslTypeFor #-}

  uniformFor 2 = glProgramUniform2fv
  uniformFor 3 = glProgramUniform3fv
  uniformFor 4 = glProgramUniform4fv
  uniformFor _ = glProgramUniform1fv
  {-# INLINE uniformFor #-}

instance Scalar Double where
  glslTypeFor 1 = "double"
  glslTypeFor n = "dvec" <> show n
  {-# INLINE glslTypeFor #-}

  uniformFor 2 = glProgramUniform2dv
  uniformFor 3 = glProgramUniform3dv
  uniformFor 4 = glProgramUniform4dv
  uniformFor _ = glProgramUniform1dv
  {-# INLINE uniformFor #-}
