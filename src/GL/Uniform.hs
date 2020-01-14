{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module GL.Uniform
( Uniform(..)
, Scalar(..)
, Col(..)
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
  glslType = glslTypePrefix @t <> "vec2"
  uniform prog loc vec = A.with vec (sendM . uniformFor @t C2x1 prog loc 1 . castPtr)

instance {-# OVERLAPPABLE #-} Scalar t => Uniform (V3 t) where
  glslType = glslTypePrefix @t <> "vec3"
  uniform prog loc vec = A.with vec (sendM . uniformFor @t C3x1 prog loc 1 . castPtr)

instance {-# OVERLAPPABLE #-} Scalar t => Uniform (V4 t) where
  glslType = glslTypePrefix @t <> "vec4"
  uniform prog loc vec = A.with vec (sendM . uniformFor @t C4x1 prog loc 1 . castPtr)

instance {-# OVERLAPPABLE #-} Scalar t => Uniform (M22 t) where
  glslType = glslTypePrefix @t <> "mat2"
  uniform prog loc matrix = A.with matrix (sendM . uniformFor @t C2x2 prog loc 1 . castPtr)

instance {-# OVERLAPPABLE #-} Scalar t => Uniform (M23 t) where
  glslType = glslTypePrefix @t <> "mat2x3"
  uniform prog loc matrix = A.with matrix (sendM . uniformFor @t C2x3 prog loc 1 . castPtr)

instance {-# OVERLAPPABLE #-} Scalar t => Uniform (M33 t) where
  glslType = glslTypePrefix @t <> "mat3"
  uniform prog loc matrix = A.with matrix (sendM . uniformFor @t C2x4 prog loc 1 . castPtr)

instance {-# OVERLAPPABLE #-} Scalar t => Uniform (M34 t) where
  glslType = glslTypePrefix @t <> "mat3x4"
  uniform prog loc matrix = A.with matrix (sendM . uniformFor @t C3x4 prog loc 1 . castPtr)

instance {-# OVERLAPPABLE #-} Scalar t => Uniform (M44 t) where
  glslType = glslTypePrefix @t <> "mat4"
  uniform prog loc matrix = A.with matrix (sendM . uniformFor @t C4x4 prog loc 1 . castPtr)

deriving instance Uniform (f a) => Uniform (Point f a)


class GL.Type t => Scalar t where
  glslTypePrefix :: String

  uniformFor :: Col -> GLuint -> GLint -> GLsizei -> Ptr t -> IO ()

data Col
  = C2x1
  | C2x2
  | C2x3
  | C2x4
  | C3x1
  | C3x2
  | C3x3
  | C3x4
  | C4x1
  | C4x2
  | C4x3
  | C4x4
  deriving (Eq, Ord, Show)

transposing :: (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr t -> IO ()) -> (GLuint -> GLint -> GLsizei -> Ptr t -> IO ())
transposing f prog loc n = f prog loc n GL_TRUE
{-# INLINE transposing #-}

instance Scalar Float where
  glslTypePrefix = ""
  {-# INLINE glslTypePrefix #-}

  uniformFor = \case
    C2x1 -> glProgramUniform2fv
    C2x2 -> transposing glProgramUniformMatrix2fv
    C2x3 -> transposing glProgramUniformMatrix2x3fv
    C2x4 -> transposing glProgramUniformMatrix2x4fv
    C3x1 -> glProgramUniform3fv
    C3x2 -> transposing glProgramUniformMatrix3x2fv
    C3x3 -> transposing glProgramUniformMatrix3fv
    C3x4 -> transposing glProgramUniformMatrix3x4fv
    C4x1 -> glProgramUniform4fv
    C4x2 -> transposing glProgramUniformMatrix4x2fv
    C4x3 -> transposing glProgramUniformMatrix4x3fv
    C4x4 -> transposing glProgramUniformMatrix4fv
  {-# INLINE uniformFor #-}

instance Scalar Double where
  glslTypePrefix = "d"
  {-# INLINE glslTypePrefix #-}

  uniformFor = \case
    C2x1 -> glProgramUniform2dv
    C2x2 -> transposing glProgramUniformMatrix2dv
    C2x3 -> transposing glProgramUniformMatrix2x3dv
    C2x4 -> transposing glProgramUniformMatrix2x4dv
    C3x1 -> glProgramUniform3dv
    C3x2 -> transposing glProgramUniformMatrix3x2dv
    C3x3 -> transposing glProgramUniformMatrix3dv
    C3x4 -> transposing glProgramUniformMatrix3x4dv
    C4x1 -> glProgramUniform4dv
    C4x2 -> transposing glProgramUniformMatrix4x2dv
    C4x3 -> transposing glProgramUniformMatrix4x3dv
    C4x4 -> transposing glProgramUniformMatrix4dv
  {-# INLINE uniformFor #-}
