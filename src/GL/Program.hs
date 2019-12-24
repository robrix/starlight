{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleInstances, FunctionalDependencies, KindSignatures, ScopedTypeVariables, TypeApplications, TypeOperators, UndecidableInstances #-}
module GL.Program
( Program(..)
, createProgram
, useProgram
, link
, checkProgram
  -- * Uniforms
, Var(..)
, setUniformValue
, HasUniform
) where

import Control.Effect.Finally
import Control.Monad.IO.Class.Lift
import Data.DSL
import Data.Foldable (for_)
import Data.Proxy
import qualified Foreign.C.String.Lift as C
import GHC.Stack
import GHC.TypeLits
import GL.Error
import GL.Shader
import GL.Uniform
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Program (ty :: Context) = Program { unProgram :: GLuint }
  deriving (Eq, Ord, Show)

createProgram :: (Has Finally sig m, Has (Lift IO) sig m) => m (Program ty)
createProgram = do
  program <- runLiftIO glCreateProgram
  Program program <$ onExit (runLiftIO (glDeleteProgram program))

useProgram :: Has (Lift IO) sig m => Program ty -> m ()
useProgram = runLiftIO . glUseProgram . unProgram

link :: (Has (Lift IO) sig m, HasCallStack) => [Shader] -> Program ty -> m ()
link shaders (Program program) = runLiftIO $ do
  for_ shaders (glAttachShader program . unShader)
  glLinkProgram program
  for_ shaders (glDetachShader program . unShader)
  checkProgram (Program program)


checkProgram :: (Has (Lift IO) sig m, HasCallStack) => Program ty -> m ()
checkProgram = runLiftIO . checkStatus glGetProgramiv glGetProgramInfoLog Other GL_LINK_STATUS . unProgram


newtype Var (name :: Symbol) t = Var t

setUniformValue :: forall name t ty m sig . (HasUniform name t ty, Has (Lift IO) sig m, HasCallStack) => Program ty -> Var name t -> m ()
setUniformValue program (Var v) = do
  location <- checkingGLError . runLiftIO $ C.withCString (symbolVal (Proxy :: Proxy name)) (glGetUniformLocation (unProgram program))
  checkingGLError $ uniform location v


class (KnownSymbol sym, Uniform t) => HasUniform (sym :: Symbol) t (tys :: Context) | sym tys -> t
instance {-# OVERLAPPABLE #-} (KnownSymbol sym, Uniform t) => HasUniform sym t (sym '::: t ': tys)
instance {-# OVERLAPPABLE #-} HasUniform sym t tys => HasUniform sym t (ty ': tys)
