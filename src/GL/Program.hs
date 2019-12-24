{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
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
, build
, use
, set
, ProgramT(..)
  -- * Uniforms
, Var(..)
, setUniformValue
, HasUniform
) where

import           Control.Algebra
import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Monad.IO.Class.Lift
import           Control.Monad.Trans.Class
import           Data.Foldable (for_)
import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Monoid (Ap(..))
import           Data.Traversable (for)
import qualified Foreign.C.String.Lift as C
import           GHC.Records
import           GHC.Stack
import           GHC.TypeLits
import           GL.Error
import           GL.Shader
import qualified GL.Shader.DSL as DSL
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


build :: forall u i o m sig . (Has Finally sig m, Has (Lift IO) sig m) => DSL.Shader u i o -> m (Program u)
build p = do
  program <- createProgram
  let s = DSL.shaderSources p
  shaders <- for s $ \ (type', source) -> do
    shader <- createShader type'
    shader <$ compile source shader
  program <$ link shaders program

use :: (Has Finally sig m, Has (Lift IO) sig m) => Program ty -> ProgramT ty m a -> m a
use p (ProgramT m) = do
  useProgram p
  a <- runReader p m
  a <$ useProgram (Program 0)

set :: (DSL.Vars ty, HasProgram ty m, Has Finally sig m, Has (Lift IO) sig m) => ty Maybe -> m ()
set v = askProgram >>= \ p ->
  getAp (getConst (DSL.foldVars (\ s -> Const . \case
    Just v  -> Ap (setUniformValue p s v)
    Nothing -> pure ()) v))



class HasProgram (ty :: (* -> *) -> *) (m :: * -> *) | m -> ty where
  askProgram :: m (Program ty)


newtype ProgramT (ty :: (* -> *) -> *) m a = ProgramT { runProgramT :: ReaderC (Program ty) m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadTrans)

instance Algebra sig m => Algebra sig (ProgramT ty m) where
  alg = ProgramT . send . handleCoercible

instance Algebra sig m => HasProgram ty (ProgramT ty m) where
  askProgram = ProgramT ask
