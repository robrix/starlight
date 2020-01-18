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
, Scalar2(..)
, RowI(..)
, ColI(..)
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

instance {-# OVERLAPPABLE #-} Scalar t => Uniform (V2 t) where
  glslType = glslTypeFor @t C2x1
  uniform prog loc vec = A.with vec (sendM . uniformFor @t C2x1 prog loc 1 . castPtr)

instance {-# OVERLAPPABLE #-} Scalar t => Uniform (V3 t) where
  glslType = glslTypeFor @t C3x1
  uniform prog loc vec = A.with vec (sendM . uniformFor @t C3x1 prog loc 1 . castPtr)

instance {-# OVERLAPPABLE #-} Scalar t => Uniform (V4 t) where
  glslType = glslTypeFor @t C4x1
  uniform prog loc vec = A.with vec (sendM . uniformFor @t C4x1 prog loc 1 . castPtr)

deriving instance Uniform (f a) => Uniform (Point f a)

deriving instance Uniform a => Uniform (Const a b)
deriving instance Uniform a => Uniform (Identity a)
deriving instance Uniform a => Uniform (K a b)


-- | Types which can appear in vectors.
--
-- NB: The name is a bit misleading because matrices are vectors containing other vectors.
class GL.Type t => Scalar t where
  glslTypeFor :: RowI -> String

  uniformFor :: RowI -> GLuint -> GLint -> GLsizei -> Ptr t -> IO ()

deriving instance Scalar a => Scalar (Const a b)
deriving instance Scalar a => Scalar (Identity a)
deriving instance Scalar a => Scalar (K a b)

data RowI
  = C2x1
  | C3x1
  | C4x1
  deriving (Enum, Eq, Ord, Show)

glslTypeForCol :: RowI -> String
glslTypeForCol = \case
  C2x1 -> "vec2"
  C3x1 -> "vec3"
  C4x1 -> "vec4"

instance Scalar Int32 where
  glslTypeFor = ('i':) . glslTypeForCol
  {-# INLINE glslTypeFor #-}

  uniformFor = \case
    C2x1 -> glProgramUniform2iv
    C3x1 -> glProgramUniform3iv
    C4x1 -> glProgramUniform4iv
  {-# INLINE uniformFor #-}

instance Scalar Float where
  glslTypeFor = glslTypeForCol
  {-# INLINE glslTypeFor #-}

  uniformFor = \case
    C2x1 -> glProgramUniform2fv
    C3x1 -> glProgramUniform3fv
    C4x1 -> glProgramUniform4fv
  {-# INLINE uniformFor #-}

instance Scalar Double where
  glslTypeFor = ('d':) . glslTypeForCol
  {-# INLINE glslTypeFor #-}

  uniformFor = \case
    C2x1 -> glProgramUniform2dv
    C3x1 -> glProgramUniform3dv
    C4x1 -> glProgramUniform4dv
  {-# INLINE uniformFor #-}

instance Scalar2 t => Scalar (V2 t) where
  glslTypeFor = glslTypeFor2 @t . replace2
  uniformFor = coerce . uniformFor2 @t . replace2

replace2 = \case
  C2x1 -> C2x2
  C3x1 -> C3x2
  C4x1 -> C4x2
{-# INLINE replace2 #-}

instance Scalar2 t => Scalar (V3 t) where
  glslTypeFor = glslTypeFor2 @t . replace3
  uniformFor = coerce . uniformFor2 @t . replace3

replace3 = \case
  C2x1 -> C2x3
  C3x1 -> C3x3
  C4x1 -> C4x3
{-# INLINE replace3 #-}

instance Scalar2 t => Scalar (V4 t) where
  glslTypeFor = glslTypeFor2 @t . replace4
  uniformFor = coerce . uniformFor2 @t . replace4

replace4 = \case
  C2x1 -> C2x4
  C3x1 -> C3x4
  C4x1 -> C4x4
{-# INLINE replace4 #-}


class Scalar t => Scalar2 t where
  glslTypeFor2 :: ColI -> String

  uniformFor2 :: ColI -> GLuint -> GLint -> GLsizei -> Ptr t -> IO ()

deriving instance Scalar2 a => Scalar2 (Const a b)
deriving instance Scalar2 a => Scalar2 (Identity a)
deriving instance Scalar2 a => Scalar2 (K a b)

data ColI
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

glslTypeForCol2 :: ColI -> String
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
