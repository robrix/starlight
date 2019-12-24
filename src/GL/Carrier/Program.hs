{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module GL.Carrier.Program
( -- * Program carrier
  runProgram
, ProgramC(..)
  -- * Program effect
, module GL.Effect.Program
) where

import Control.Algebra
import Control.Effect.Finally
import Control.Monad.IO.Class.Lift
import Data.Traversable (for)
import GL.Effect.Program
import GL.Shader
import GL.Shader.DSL (progShaders)
import qualified GL.Program as GL

runProgram :: ProgramC m a -> m a
runProgram (ProgramC m) = m

newtype ProgramC m a = ProgramC (m a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance (Has Finally sig m, Has (Lift IO) sig m) => Algebra (Program :+: sig) (ProgramC m) where
  alg = \case
    L (Build' p k) -> do
      program <- GL.createProgram
      let s = progShaders p
      shaders <- for s $ \ (type', source) -> do
        shader <- createShader type'
        shader <$ compile source shader
      GL.link shaders program
      k program
    L (Build s k) -> do
      program <- GL.createProgram
      shaders <- for s $ \ (type', path) -> do
        shader <- createShader type'
        source <- sendM (readFile path)
        shader <$ compile source shader
      GL.link shaders program
      k program
    L (Use p m k) -> do
      GL.useProgram p
      a <- m
      GL.useProgram (GL.Program 0)
      k a
    L (Set p v k) -> GL.setUniformValue p v >> k
    R other       -> ProgramC (send (handleCoercible other))
