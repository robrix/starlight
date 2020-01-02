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
  uniform :: (HasCallStack, Has (Lift IO) sig m) => GLint -> t -> m ()

instance Uniform Bool where
  uniform loc = runLiftIO . glUniform1i loc . fromIntegral . fromEnum

instance Uniform Int where
  uniform loc = runLiftIO . glUniform1i loc . fromIntegral

instance Uniform Int32 where
  uniform loc = runLiftIO . glUniform1i loc

instance Uniform Float where
  uniform loc = runLiftIO . glUniform1f loc

instance Uniform Double where
  uniform loc = runLiftIO . glUniform1d loc

instance Uniform (Linear.V2 Float) where
  uniform loc (Linear.V2 x y) = runLiftIO $ glUniform2f loc x y

instance Uniform (Linear.V2 Double) where
  uniform loc (Linear.V2 x y) = runLiftIO $ glUniform2d loc x y

instance Uniform (Linear.V3 Float) where
  uniform loc (Linear.V3 x y z) = runLiftIO $ glUniform3f loc x y z

instance Uniform (Linear.V3 Double) where
  uniform loc (Linear.V3 x y z) = runLiftIO $ glUniform3d loc x y z

instance Uniform (Linear.V4 Float) where
  uniform loc (Linear.V4 x y z w) = runLiftIO $ glUniform4f loc x y z w

instance Uniform (Linear.V4 Double) where
  uniform loc (Linear.V4 x y z w) = runLiftIO $ glUniform4d loc x y z w

instance Uniform (Linear.M22 Float) where
  uniform loc matrix = A.with (Linear.transpose matrix) (runLiftIO . glUniformMatrix2fv loc 1 GL_FALSE . castPtr)

instance Uniform (Linear.M22 Double) where
  uniform loc matrix = A.with (Linear.transpose matrix) (runLiftIO . glUniformMatrix2dv loc 1 GL_FALSE . castPtr)

instance Uniform (Linear.M33 Float) where
  uniform loc matrix = A.with (Linear.transpose matrix) (runLiftIO . glUniformMatrix3fv loc 1 GL_FALSE . castPtr)

instance Uniform (Linear.M33 Double) where
  uniform loc matrix = A.with (Linear.transpose matrix) (runLiftIO . glUniformMatrix3dv loc 1 GL_FALSE . castPtr)

instance Uniform (Linear.M44 Float) where
  uniform loc matrix = A.with (Linear.transpose matrix) (runLiftIO . glUniformMatrix4fv loc 1 GL_FALSE . castPtr)

instance Uniform (Linear.M44 Double) where
  uniform loc matrix = A.with (Linear.transpose matrix) (runLiftIO . glUniformMatrix4dv loc 1 GL_FALSE . castPtr)

deriving instance Uniform (f a) => Uniform (Linear.Point f a)
