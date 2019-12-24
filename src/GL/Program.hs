{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
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

import           Control.Effect.Finally
import           Control.Monad.IO.Class.Lift
import           Data.Foldable (for_)
import           Data.Functor.Identity
import qualified Foreign.C.String.Lift as C
import           GHC.Records
import           GHC.Stack
import           GHC.TypeLits
import           GL.Error
import           GL.Shader
import           GL.Uniform
import           Graphics.GL.Core41
import           Graphics.GL.Types

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

setUniformValue :: (Uniform t, Has (Lift IO) sig m, HasCallStack) => Program ty -> String -> t -> m ()
setUniformValue program name v = do
  location <- checkingGLError . runLiftIO $ C.withCString name (glGetUniformLocation (unProgram program))
  checkingGLError $ uniform location v


type HasUniform sym t ty = (KnownSymbol sym, Uniform t, HasField sym (ty Identity) (Identity t))
