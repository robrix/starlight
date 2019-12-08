{-# LANGUAGE DataKinds, FlexibleInstances, KindSignatures, MultiParamTypeClasses, ScopedTypeVariables, TypeOperators #-}
module GL.Uniform
( Var(..)
, Uniform(..)
, setUniformValue
, HasVar
) where

import Control.Monad.IO.Class.Lift
import Data.Foldable (toList)
import Data.Proxy
import qualified Foreign.C.String.Lift as C
import qualified Foreign.Marshal.Array.Lift as A
import Foreign.Ptr
import GHC.Stack
import GHC.TypeLits
import GL.Error
import GL.Program
import Graphics.GL.Core41
import Graphics.GL.Types
import Linear.Matrix as Linear
import Linear.V4 as Linear

data Var (name :: Symbol) t = Var

class Uniform t where
  uniform :: Has (Lift IO) sig m => HasCallStack => GLint -> t -> m ()

setUniformValue :: forall name t ty m sig . (KnownSymbol name, Uniform t, Has (Lift IO) sig m, HasCallStack) => Program ty -> Var name t -> t -> m ()
setUniformValue program _ v = do
  location <- checkingGLError . runLiftIO $ C.withCString (symbolVal (Proxy :: Proxy name)) (glGetUniformLocation (unProgram program))
  checkingGLError $ uniform location v

instance Uniform (Linear.V4 Float) where
  uniform location (Linear.V4 x y z w) = runLiftIO $ glUniform4f location x y z w

instance Uniform (Linear.V4 Double) where
  uniform location (Linear.V4 x y z w) = runLiftIO $ glUniform4d location x y z w

instance Uniform (Linear.M44 Float) where
  uniform location matrix = A.withArray (toList (Linear.transpose matrix) >>= toList) (runLiftIO . glUniformMatrix4fv location 1 GL_FALSE . castPtr)

instance Uniform (Linear.M33 Float) where
  uniform location matrix = A.withArray (toList (Linear.transpose matrix) >>= toList) (runLiftIO . glUniformMatrix3fv location 1 GL_FALSE . castPtr)


class KnownSymbol sym => HasVar (sig :: [Symbol ::: *]) (sym :: Symbol) t

instance {-# OVERLAPPABLE #-} KnownSymbol sym => HasVar (sym '::: t ': tys) sym t
instance {-# OVERLAPPABLE #-} HasVar tys sym t => HasVar (ty ': tys) sym t
