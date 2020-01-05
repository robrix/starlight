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
, HasProgram(..)
, ProgramC(..)
  -- * Uniforms
, HasUniform
) where

import           Control.Algebra
import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.State
import           Control.Monad.IO.Class.Lift
import           Control.Monad.Trans.Class
import           Data.Foldable (for_)
import           Data.Functor.Identity
import qualified Data.IntMap as IntMap
import           Data.Traversable (for)
import qualified Foreign.C.String.Lift as C
import           GHC.Records
import           GHC.Stack
import           GHC.TypeLits
import           GL.Effect.Check
import           GL.Error
import           GL.Shader
import qualified GL.Shader.DSL as DSL
import           GL.Uniform
import           Graphics.GL.Core41
import           Graphics.GL.Types

data Program (u :: (* -> *) -> *) (i :: (* -> *) -> *) (o :: (* -> *) -> *) = Program
  { locations :: IntMap.IntMap GLint
  , unProgram :: GLuint
  }

type HasUniform sym t u = (KnownSymbol sym, Uniform t, HasField sym (u Identity) (Identity t))


build :: forall u i o m sig . (HasCallStack, Has Check sig m, Has Finally sig m, Has (Lift IO) sig m, DSL.Vars u, DSL.Vars i) => DSL.Shader u i o -> m (Program u i o)
build p = runLiftIO $ do
  program <- glCreateProgram
  onExit (glDeleteProgram program)
  DSL.foldVarsM @i (\ DSL.Field { DSL.name, DSL.location } -> checking $
    C.withCString name (glBindAttribLocation program (fromIntegral location))) DSL.defaultVars
  shaders <- for (DSL.shaderSources p) $ \ (type', source) -> do
    shader <- createShader type'
    shader <$ compile source shader

  for_ shaders (glAttachShader program . unShader)
  glLinkProgram program
  for_ shaders (glDetachShader program . unShader)

  checkStatus glGetProgramiv glGetProgramInfoLog Other GL_LINK_STATUS program

  ls <- DSL.foldVarsM @u (\ DSL.Field{ DSL.name, DSL.location } -> do
    loc <- checking $ C.withCString name (glGetUniformLocation program)
    pure (IntMap.singleton location loc)) DSL.defaultVars

  pure (Program ls program)

use :: Has (Lift IO) sig m => Program u i o -> ProgramC u i o m a -> m a
use (Program ls p) (ProgramC m) = do
  sendIO (glUseProgram p)
  a <- runReader (Program ls p) m
  a <$ sendIO (glUseProgram 0)


class Monad m => HasProgram (u :: (* -> *) -> *) (i :: (* -> *) -> *) (o :: (* -> *) -> *) (m :: * -> *) | m -> u i o where
  askProgram :: m (Program u i o)


newtype ProgramC (u :: (* -> *) -> *) (i :: (* -> *) -> *) (o :: (* -> *) -> *) m a = ProgramC { runProgramT :: ReaderC (Program u i o) m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadTrans)

instance HasProgram u i o m => HasProgram u i o (ReaderC r m) where
  askProgram = lift askProgram

instance (Has Check sig m, Has (Lift IO) sig m, DSL.Vars u) => Algebra (State (u Maybe) :+: sig) (ProgramC u i o m) where
  alg = \case
    L (Get   k) -> k DSL.defaultVars
    L (Put s k) -> do
      Program ls prog <- askProgram
      DSL.foldVarsM (\ DSL.Field { DSL.location, DSL.value } ->
        maybe (pure ()) (checking . uniform prog (ls IntMap.! location)) value) s
      k
    R other     -> ProgramC (send (handleCoercible other))

instance Algebra sig m => HasProgram u i o (ProgramC u i o m) where
  askProgram = ProgramC ask
