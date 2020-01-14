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
, N(..)
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
  uniform prog loc vec = A.with vec (sendM . uniformFor @t N2 Nothing prog loc 1 . castPtr)

instance {-# OVERLAPPABLE #-} Scalar t => Uniform (V3 t) where
  glslType = glslTypePrefix @t <> "vec3"
  uniform prog loc vec = A.with vec (sendM . uniformFor @t N3 Nothing prog loc 1 . castPtr)

instance {-# OVERLAPPABLE #-} Scalar t => Uniform (V4 t) where
  glslType = glslTypePrefix @t <> "vec4"
  uniform prog loc vec = A.with vec (sendM . uniformFor @t N4 Nothing prog loc 1 . castPtr)

instance {-# OVERLAPPABLE #-} Scalar t => Uniform (M22 t) where
  glslType = glslTypePrefix @t <> "mat2"
  uniform prog loc matrix = A.with matrix (sendM . uniformFor @t N2 (Just N2) prog loc 1 . castPtr)

instance {-# OVERLAPPABLE #-} Scalar t => Uniform (M23 t) where
  glslType = glslTypePrefix @t <> "mat2x3"
  uniform prog loc matrix = A.with matrix (sendM . uniformFor @t N2 (Just N3) prog loc 1 . castPtr)

instance {-# OVERLAPPABLE #-} Scalar t => Uniform (M33 t) where
  glslType = glslTypePrefix @t <> "mat3"
  uniform prog loc matrix = A.with matrix (sendM . uniformFor @t N3 (Just N3) prog loc 1 . castPtr)

instance {-# OVERLAPPABLE #-} Scalar t => Uniform (M34 t) where
  glslType = glslTypePrefix @t <> "mat3x4"
  uniform prog loc matrix = A.with matrix (sendM . uniformFor @t N3 (Just N4) prog loc 1 . castPtr)

instance {-# OVERLAPPABLE #-} Scalar t => Uniform (M44 t) where
  glslType = glslTypePrefix @t <> "mat4"
  uniform prog loc matrix = A.with matrix (sendM . uniformFor @t N4 (Just N4) prog loc 1 . castPtr)

deriving instance Uniform (f a) => Uniform (Point f a)


class GL.Type t => Scalar t where
  glslTypePrefix :: String

  uniformFor :: N -> Maybe N -> GLuint -> GLint -> GLsizei -> Ptr t -> IO ()

data N = N2 | N3 | N4
  deriving (Eq, Ord, Show)

transposing :: (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr t -> IO ()) -> (GLuint -> GLint -> GLsizei -> Ptr t -> IO ())
transposing f prog loc n = f prog loc n GL_TRUE
{-# INLINE transposing #-}

instance Scalar Float where
  glslTypePrefix = ""
  {-# INLINE glslTypePrefix #-}

  uniformFor m n = case (m, n) of
    (N2, Nothing) -> glProgramUniform2fv
    (N3, Nothing) -> glProgramUniform3fv
    (N4, Nothing) -> glProgramUniform4fv
    (N2, Just N2) -> transposing glProgramUniformMatrix2fv
    (N2, Just N3) -> transposing glProgramUniformMatrix2x3fv
    (N2, Just N4) -> transposing glProgramUniformMatrix2x4fv
    (N3, Just N2) -> transposing glProgramUniformMatrix3x2fv
    (N3, Just N3) -> transposing glProgramUniformMatrix3fv
    (N3, Just N4) -> transposing glProgramUniformMatrix3x4fv
    (N4, Just N2) -> transposing glProgramUniformMatrix4x2fv
    (N4, Just N3) -> transposing glProgramUniformMatrix4x3fv
    (N4, Just N4) -> transposing glProgramUniformMatrix4fv
  {-# INLINE uniformFor #-}

instance Scalar Double where
  glslTypePrefix = "d"
  {-# INLINE glslTypePrefix #-}

  uniformFor m n = case (m, n) of
    (N2, Nothing) -> glProgramUniform2dv
    (N3, Nothing) -> glProgramUniform3dv
    (N4, Nothing) -> glProgramUniform4dv
    (N2, Just N2) -> transposing glProgramUniformMatrix2dv
    (N2, Just N3) -> transposing glProgramUniformMatrix2x3dv
    (N2, Just N4) -> transposing glProgramUniformMatrix2x4dv
    (N3, Just N2) -> transposing glProgramUniformMatrix3x2dv
    (N3, Just N3) -> transposing glProgramUniformMatrix3dv
    (N3, Just N4) -> transposing glProgramUniformMatrix3x4dv
    (N4, Just N2) -> transposing glProgramUniformMatrix4x2dv
    (N4, Just N3) -> transposing glProgramUniformMatrix4x3dv
    (N4, Just N4) -> transposing glProgramUniformMatrix4dv
  {-# INLINE uniformFor #-}
