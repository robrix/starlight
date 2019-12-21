{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module GL.Uniform
( Uniform(..)
) where

import Control.Monad.IO.Class.Lift
import qualified Foreign.Marshal.Utils.Lift as A
import Foreign.Ptr
import GHC.Stack
import Graphics.GL.Core41
import Graphics.GL.Types
import Linear.Affine as Linear
import Linear.Matrix as Linear
import Linear.V2 as Linear
import Linear.V3 as Linear
import Linear.V4 as Linear

class Uniform t where
  uniform :: Has (Lift IO) sig m => HasCallStack => GLint -> t -> m ()

instance Uniform Float where
  uniform = fmap runLiftIO . glUniform1f

instance Uniform Double where
  uniform = fmap runLiftIO . glUniform1d

instance Uniform (Linear.V2 Float) where
  uniform location (Linear.V2 x y) = runLiftIO $ glUniform2f location x y

instance Uniform (Linear.V2 Double) where
  uniform location (Linear.V2 x y) = runLiftIO $ glUniform2d location x y

instance Uniform (Linear.V3 Float) where
  uniform location (Linear.V3 x y z) = runLiftIO $ glUniform3f location x y z

instance Uniform (Linear.V4 Float) where
  uniform location (Linear.V4 x y z w) = runLiftIO $ glUniform4f location x y z w

instance Uniform (Linear.V4 Double) where
  uniform location (Linear.V4 x y z w) = runLiftIO $ glUniform4d location x y z w

instance Uniform (Linear.M44 Float) where
  uniform location matrix = A.with (Linear.transpose matrix) (runLiftIO . glUniformMatrix4fv location 1 GL_FALSE . castPtr)

instance Uniform (Linear.M33 Float) where
  uniform location matrix = A.with (Linear.transpose matrix) (runLiftIO . glUniformMatrix3fv location 1 GL_FALSE . castPtr)

deriving instance Uniform (f a) => Uniform (Linear.Point f a)
