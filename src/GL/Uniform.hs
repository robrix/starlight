{-# LANGUAGE FlexibleInstances #-}
module GL.Uniform
( Var(..)
, Uniform(..)
, setUniformValue
) where

import Control.Monad.IO.Class.Lift
import Data.Foldable (toList)
import qualified Foreign.C.String.Lift as C
import qualified Foreign.Marshal.Array.Lift as A
import Foreign.Ptr
import GHC.Stack
import GL.Error
import GL.Program
import Graphics.GL.Core41
import Graphics.GL.Types
import Linear.Matrix as Linear
import Linear.V4 as Linear

newtype Var t = Var { varName :: String }

class Uniform t where
  uniform :: Has (Lift IO) sig m => HasCallStack => GLint -> t -> m ()

setUniformValue :: (Uniform t, Has (Lift IO) sig m, HasCallStack) => Program -> Var t -> t -> m ()
setUniformValue program var v = do
  location <- checkingGLError . runLifting $ C.withCString (varName var) (glGetUniformLocation (unProgram program))
  checkingGLError $ uniform location v

instance Uniform (Linear.V4 Float) where
  uniform location (Linear.V4 x y z w) = runLifting $ glUniform4f location x y z w

instance Uniform (Linear.V4 Double) where
  uniform location (Linear.V4 x y z w) = runLifting $ glUniform4d location x y z w

instance Uniform (Linear.M44 Float) where
  uniform location matrix = A.withArray (toList (Linear.transpose matrix) >>= toList) (runLifting . glUniformMatrix4fv location 1 GL_FALSE . castPtr)

instance Uniform (Linear.M33 Float) where
  uniform location matrix = A.withArray (toList (Linear.transpose matrix) >>= toList) (runLifting . glUniformMatrix3fv location 1 GL_FALSE . castPtr)
