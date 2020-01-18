{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module GL.Uniform
( Uniform(..)
, Row(..)
, Scalar2(..)
, RowD(..)
, ColD(..)
) where

import           Control.Monad.IO.Class.Lift
import           Data.Coerce
import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Functor.K
import           Data.Int
import qualified Foreign.Marshal.Utils.Lift as A
import           Foreign.Ptr
import           GHC.Stack
import qualified GL.Type as GL
import           Graphics.GL.Core41
import           Graphics.GL.Types
import           Linear.Affine
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

instance {-# OVERLAPPABLE #-} Row t => Uniform (V2 t) where
  glslType = glslTypeFor @t R2
  uniform prog loc vec = A.with vec (sendM . uniformFor @t R2 prog loc 1 . castPtr)

instance {-# OVERLAPPABLE #-} Row t => Uniform (V3 t) where
  glslType = glslTypeFor @t R3
  uniform prog loc vec = A.with vec (sendM . uniformFor @t R3 prog loc 1 . castPtr)

instance {-# OVERLAPPABLE #-} Row t => Uniform (V4 t) where
  glslType = glslTypeFor @t R4
  uniform prog loc vec = A.with vec (sendM . uniformFor @t R4 prog loc 1 . castPtr)

deriving instance Uniform (f a) => Uniform (Point f a)

deriving instance Uniform a => Uniform (Const a b)
deriving instance Uniform a => Uniform (Identity a)
deriving instance Uniform a => Uniform (K a b)


-- | Types which can appear in vectors.
class GL.Type t => Row t where
  glslTypeFor :: RowD -> String

  uniformFor :: RowD -> GLuint -> GLint -> GLsizei -> Ptr t -> IO ()

deriving instance Row a => Row (Const a b)
deriving instance Row a => Row (Identity a)
deriving instance Row a => Row (K a b)

data RowD
  = R2
  | R3
  | R4
  deriving (Enum, Eq, Ord, Show)

glslTypeForCol :: RowD -> String
glslTypeForCol = \case
  R2 -> "vec2"
  R3 -> "vec3"
  R4 -> "vec4"

instance Row Int32 where
  glslTypeFor = ('i':) . glslTypeForCol
  {-# INLINE glslTypeFor #-}

  uniformFor = \case
    R2 -> glProgramUniform2iv
    R3 -> glProgramUniform3iv
    R4 -> glProgramUniform4iv
  {-# INLINE uniformFor #-}

instance Row Float where
  glslTypeFor = glslTypeForCol
  {-# INLINE glslTypeFor #-}

  uniformFor = \case
    R2 -> glProgramUniform2fv
    R3 -> glProgramUniform3fv
    R4 -> glProgramUniform4fv
  {-# INLINE uniformFor #-}

instance Row Double where
  glslTypeFor = ('d':) . glslTypeForCol
  {-# INLINE glslTypeFor #-}

  uniformFor = \case
    R2 -> glProgramUniform2dv
    R3 -> glProgramUniform3dv
    R4 -> glProgramUniform4dv
  {-# INLINE uniformFor #-}

instance Scalar2 t => Row (V2 t) where
  glslTypeFor = glslTypeFor2 @t . replace2
  uniformFor = coerce . uniformFor2 @t . replace2

replace2 = \case
  R2 -> C2x2
  R3 -> C3x2
  R4 -> C4x2
{-# INLINE replace2 #-}

instance Scalar2 t => Row (V3 t) where
  glslTypeFor = glslTypeFor2 @t . replace3
  uniformFor = coerce . uniformFor2 @t . replace3

replace3 = \case
  R2 -> C2x3
  R3 -> C3x3
  R4 -> C4x3
{-# INLINE replace3 #-}

instance Scalar2 t => Row (V4 t) where
  glslTypeFor = glslTypeFor2 @t . replace4
  uniformFor = coerce . uniformFor2 @t . replace4

replace4 = \case
  R2 -> C2x4
  R3 -> C3x4
  R4 -> C4x4
{-# INLINE replace4 #-}


class Row t => Scalar2 t where
  glslTypeFor2 :: ColD -> String

  uniformFor2 :: ColD -> GLuint -> GLint -> GLsizei -> Ptr t -> IO ()

deriving instance Scalar2 a => Scalar2 (Const a b)
deriving instance Scalar2 a => Scalar2 (Identity a)
deriving instance Scalar2 a => Scalar2 (K a b)

data ColD
  = C2x2
  | C2x3
  | C2x4
  | C3x2
  | C3x3
  | C3x4
  | C4x2
  | C4x3
  | C4x4
  deriving (Enum, Eq, Ord, Show)

glslTypeForCol2 :: ColD -> String
glslTypeForCol2 = \case
  C2x2 -> "mat2"
  C2x3 -> "mat2x3"
  C2x4 -> "mat2x4"
  C3x2 -> "mat3x2"
  C3x3 -> "mat3"
  C3x4 -> "mat3x4"
  C4x2 -> "mat4x2"
  C4x3 -> "mat4x3"
  C4x4 -> "mat4"

transposing :: (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr t -> IO ()) -> (GLuint -> GLint -> GLsizei -> Ptr t -> IO ())
transposing f prog loc n = f prog loc n GL_TRUE
{-# INLINE transposing #-}

instance Scalar2 Float where
  glslTypeFor2 = glslTypeForCol2
  {-# INLINE glslTypeFor2 #-}

  uniformFor2 = \case
    C2x2 -> transposing glProgramUniformMatrix2fv
    C2x3 -> transposing glProgramUniformMatrix2x3fv
    C2x4 -> transposing glProgramUniformMatrix2x4fv
    C3x2 -> transposing glProgramUniformMatrix3x2fv
    C3x3 -> transposing glProgramUniformMatrix3fv
    C3x4 -> transposing glProgramUniformMatrix3x4fv
    C4x2 -> transposing glProgramUniformMatrix4x2fv
    C4x3 -> transposing glProgramUniformMatrix4x3fv
    C4x4 -> transposing glProgramUniformMatrix4fv
  {-# INLINE uniformFor2 #-}

instance Scalar2 Double where
  glslTypeFor2 = ('d':) . glslTypeForCol2
  {-# INLINE glslTypeFor2 #-}

  uniformFor2 = \case
    C2x2 -> transposing glProgramUniformMatrix2dv
    C2x3 -> transposing glProgramUniformMatrix2x3dv
    C2x4 -> transposing glProgramUniformMatrix2x4dv
    C3x2 -> transposing glProgramUniformMatrix3x2dv
    C3x3 -> transposing glProgramUniformMatrix3dv
    C3x4 -> transposing glProgramUniformMatrix3x4dv
    C4x2 -> transposing glProgramUniformMatrix4x2dv
    C4x3 -> transposing glProgramUniformMatrix4x3dv
    C4x4 -> transposing glProgramUniformMatrix4dv
  {-# INLINE uniformFor2 #-}
