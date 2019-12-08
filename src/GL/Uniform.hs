{-# LANGUAGE DataKinds, FlexibleInstances, FunctionalDependencies, KindSignatures, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module GL.Uniform
( Var(..)
, Uniform(..)
, setUniformValue
, HasUniform
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

newtype Var (name :: Symbol) t = Var t

class Uniform t where
  uniform :: Has (Lift IO) sig m => HasCallStack => GLint -> t -> m ()

setUniformValue :: forall name t ty m sig . (HasUniform ty name t, Uniform t, Has (Lift IO) sig m, HasCallStack) => Program ty -> Var name t -> m ()
setUniformValue program (Var v) = do
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


class KnownSymbol sym => HasUniform (sig :: [Symbol ::: *]) (sym :: Symbol) t | sym sig -> t

instance {-# OVERLAPPABLE #-} KnownSymbol sym => HasUniform (sym '::: t ': tys) sym t
instance {-# OVERLAPPABLE #-} HasUniform tys sym t => HasUniform (ty ': tys) sym t
