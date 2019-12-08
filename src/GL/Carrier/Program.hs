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
import qualified GL.Enum as GL
import GL.Shader
import qualified GL.Program as GL
import GL.Uniform
import Graphics.GL.Core41

runProgram :: ProgramC m a -> m a
runProgram (ProgramC m) = m

newtype ProgramC m a = ProgramC (m a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance (Has Finally sig m, Has (Lift IO) sig m) => Algebra (Program :+: sig) (ProgramC m) where
  alg = \case
    L (Build s   k) -> do
      program <- GL.createProgram
      shaders <- for s $ \ (type', path) -> do
        shader <- Shader <$> runLiftIO (glCreateShader (GL.glEnum type'))
        onExit $ runLiftIO (glDeleteShader (unShader shader))
        source <- sendM (readFile path)
        shader <$ compile source shader
      GL.link shaders program
      k program
    L (Use p     k) -> GL.useProgram p >> k
    L (Set p v a k) -> setUniformValue p v a >> k
    R other         -> ProgramC (send (handleCoercible other))
