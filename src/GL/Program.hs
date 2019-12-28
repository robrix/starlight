{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module GL.Program
( Program(..)
, build
, use
, set
, HasProgram(..)
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
import           Data.Functor.Identity
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

newtype Program (u :: (* -> *) -> *) (i :: (* -> *) -> *) (o :: (* -> *) -> *) = Program { unProgram :: GLuint }
  deriving (Eq, Ord, Show)

createProgram :: (Has Finally sig m, Has (Lift IO) sig m) => m (Program u i o)
createProgram = do
  program <- runLiftIO glCreateProgram
  Program program <$ onExit (runLiftIO (glDeleteProgram program))

useProgram :: Has (Lift IO) sig m => Program u i o -> m ()
useProgram = runLiftIO . glUseProgram . unProgram

link :: (Has (Lift IO) sig m, HasCallStack) => [Shader] -> Program u i o -> m ()
link shaders (Program program) = runLiftIO $ do
  for_ shaders (glAttachShader program . unShader)
  glLinkProgram program
  for_ shaders (glDetachShader program . unShader)
  checkProgram (Program program)


checkProgram :: (Has (Lift IO) sig m, HasCallStack) => Program u i o -> m ()
checkProgram = runLiftIO . checkStatus glGetProgramiv glGetProgramInfoLog Other GL_LINK_STATUS . unProgram


newtype Var (name :: Symbol) t = Var t

setUniformValue :: (Uniform t, Has (Lift IO) sig m, HasCallStack) => Program u i o -> String -> t -> m ()
setUniformValue program name v = do
  location <- checkingGLError . runLiftIO $ C.withCString name (glGetUniformLocation (unProgram program))
  checkingGLError $ uniform location v


type HasUniform sym t u = (KnownSymbol sym, Uniform t, HasField sym (u Identity) (Identity t))


build :: forall u i o m sig . (Has Finally sig m, Has (Lift IO) sig m, DSL.Vars i) => DSL.Shader u i o -> m (Program u i o)
build p = do
  program <- createProgram
  DSL.foldVarsM @i (\ DSL.Field { DSL.name, DSL.location } _ -> runLiftIO . checkingGLError $
    C.withCString name (glBindAttribLocation (unProgram program) (fromIntegral location))) (DSL.makeVars id)
  let s = DSL.shaderSources p
  shaders <- for s $ \ (type', source) -> do
    shader <- createShader type'
    shader <$ compile source shader
  program <$ link shaders program

use :: (Has (Lift IO) sig m) => Program u i o -> ProgramT u i o m a -> m a
use p (ProgramT m) = do
  useProgram p
  a <- runReader p m
  a <$ useProgram (Program 0)

set :: (DSL.Vars u, HasProgram u i o m, Has (Lift IO) sig m) => u Maybe -> m ()
set v = askProgram >>= \ p ->
  DSL.foldVarsM (\ DSL.Field { DSL.name } -> \case
    Just v  -> setUniformValue p name v
    Nothing -> pure ()) v


class Monad m => HasProgram (u :: (* -> *) -> *) (i :: (* -> *) -> *) (o :: (* -> *) -> *) (m :: * -> *) | m -> u i o where
  askProgram :: m (Program u i o)


newtype ProgramT (u :: (* -> *) -> *) (i :: (* -> *) -> *) (o :: (* -> *) -> *) m a = ProgramT { runProgramT :: ReaderC (Program u i o) m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadTrans)

instance HasProgram u i o m => HasProgram u i o (ReaderC r m) where
  askProgram = lift askProgram

instance Algebra sig m => Algebra sig (ProgramT u i o m) where
  alg = ProgramT . send . handleCoercible

instance Algebra sig m => HasProgram u i o (ProgramT u i o m) where
  askProgram = ProgramT ask
