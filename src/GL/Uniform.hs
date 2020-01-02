{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module GL.Uniform
( Uniform(..)
) where

import           Control.Monad.IO.Class.Lift
import           Data.Int
import qualified Foreign.Marshal.Utils.Lift as A
import           Foreign.Ptr
import           GHC.Stack
import qualified GL.Type as GL
import           Graphics.GL.Core41
import           Graphics.GL.Types
import           Linear.Affine as Linear
import           Linear.Matrix as Linear
import           Linear.V2 as Linear
import           Linear.V3 as Linear
import           Linear.V4 as Linear

class GL.Type t => Uniform t where
  uniform :: (HasCallStack, Has (Lift IO) sig m) => GLuint -> GLint -> t -> m ()

instance Uniform Bool where
  uniform prog loc = runLiftIO . glProgramUniform1i prog loc . fromIntegral . fromEnum

instance Uniform Int where
  uniform prog loc = runLiftIO . glProgramUniform1i prog loc . fromIntegral

instance Uniform Int32 where
  uniform prog loc = runLiftIO . glProgramUniform1i prog loc

instance Uniform Float where
  uniform prog loc = runLiftIO . glProgramUniform1f prog loc

instance Uniform Double where
  uniform prog loc = runLiftIO . glProgramUniform1d prog loc

instance Uniform (Linear.V2 Float) where
  uniform prog loc (Linear.V2 x y) = runLiftIO $ glProgramUniform2f prog loc x y

instance Uniform (Linear.V2 Double) where
  uniform prog loc (Linear.V2 x y) = runLiftIO $ glProgramUniform2d prog loc x y

instance Uniform (Linear.V3 Float) where
  uniform prog loc (Linear.V3 x y z) = runLiftIO $ glProgramUniform3f prog loc x y z

instance Uniform (Linear.V3 Double) where
  uniform prog loc (Linear.V3 x y z) = runLiftIO $ glProgramUniform3d prog loc x y z

instance Uniform (Linear.V4 Float) where
  uniform prog loc (Linear.V4 x y z w) = runLiftIO $ glProgramUniform4f prog loc x y z w

instance Uniform (Linear.V4 Double) where
  uniform prog loc (Linear.V4 x y z w) = runLiftIO $ glProgramUniform4d prog loc x y z w

instance Uniform (Linear.M22 Float) where
  uniform prog loc matrix = A.with (Linear.transpose matrix) (runLiftIO . glProgramUniformMatrix2fv prog loc 1 GL_FALSE . castPtr)

instance Uniform (Linear.M22 Double) where
  uniform prog loc matrix = A.with (Linear.transpose matrix) (runLiftIO . glProgramUniformMatrix2dv prog loc 1 GL_FALSE . castPtr)

instance Uniform (Linear.M33 Float) where
  uniform prog loc matrix = A.with (Linear.transpose matrix) (runLiftIO . glProgramUniformMatrix3fv prog loc 1 GL_FALSE . castPtr)

instance Uniform (Linear.M33 Double) where
  uniform prog loc matrix = A.with (Linear.transpose matrix) (runLiftIO . glProgramUniformMatrix3dv prog loc 1 GL_FALSE . castPtr)

instance Uniform (Linear.M44 Float) where
  uniform prog loc matrix = A.with (Linear.transpose matrix) (runLiftIO . glProgramUniformMatrix4fv prog loc 1 GL_FALSE . castPtr)

instance Uniform (Linear.M44 Double) where
  uniform prog loc matrix = A.with (Linear.transpose matrix) (runLiftIO . glProgramUniformMatrix4dv prog loc 1 GL_FALSE . castPtr)

deriving instance Uniform (f a) => Uniform (Linear.Point f a)
