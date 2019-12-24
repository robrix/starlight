{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, KindSignatures, ScopedTypeVariables, TypeApplications, TypeOperators, UndecidableInstances #-}
module GL.Program
( Program(..)
, createProgram
, useProgram
, link
, checkProgram
  -- * Uniforms
, (:::)(..)
, Var(..)
, setUniformValue
, HasUniform
, Rec(..)
) where

import Control.Effect.Finally
import Control.Monad.IO.Class.Lift
import Data.DSL
import Data.Foldable (for_)
import Data.Functor.Identity
import Data.Proxy
import qualified Foreign.C.String.Lift as C
import GHC.Records
import GHC.Stack
import GHC.TypeLits
import GL.Error
import GL.Shader
import GL.Uniform
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Program (ty :: (* -> *) -> *) = Program { unProgram :: GLuint }
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
  location <- checkingGLError . runLiftIO $ C.withCString (symbolVal (Proxy @name)) (glGetUniformLocation (unProgram program))
  checkingGLError $ uniform location v


type HasUniform sym t ty = (KnownSymbol sym, Uniform t, HasField sym (ty Identity) (Identity t))


data Rec (ts :: Context) (v :: * -> *) where
  (:.) :: v t -> Rec ts v -> Rec (n '::: t ': ts) v
  Nil :: Rec '[] v

infixr 4 :.


instance {-# OVERLAPPABLE #-}
         HasField (sym :: Symbol) (Rec (sym '::: t ': tys) f) (f t) where
  getField (h :. _) = h

instance {-# OVERLAPPABLE #-}
         HasField sym (Rec tys f) (f t)
      => HasField (sym :: Symbol) (Rec (other ': tys) f) (f t) where
  getField (_ :. t) = getField @sym t
