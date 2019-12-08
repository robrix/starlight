{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module GL.Carrier.Program.Live
( -- * Program carrier
  runProgram
, ProgramC(..)
  -- * Program effect
, module GL.Effect.Program
) where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.State.Strict
import Control.Effect.Finally
import Control.Monad (when)
import Control.Monad.IO.Class.Lift
import qualified Data.Map as Map
import Data.Time.Clock (UTCTime)
import Data.Traversable (for)
import GL.Effect.Program
import qualified GL.Enum as GL
import GL.Shader
import qualified GL.Program as GL
import GL.Uniform
import Graphics.GL.Core41
import System.Directory

runProgram :: Has (Lift IO) sig m => ProgramC m a -> m a
runProgram (ProgramC m) = evalState Map.empty m

newtype ProgramC m a = ProgramC (StateC (Map.Map GL.Program [ShaderState]) m a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance (Has Finally sig m, Has (Lift IO) sig m, Effect sig) => Algebra (Program :+: sig) (ProgramC m) where
  alg = \case
    L (Build s   k) -> do
      program <- GL.createProgram
      shaders <- for s $ \ (type', path) -> do
        shader <- Shader <$> runLiftIO (glCreateShader (GL.glEnum type'))
        onExit $ runLiftIO (glDeleteShader (unShader shader))
        pure $! ShaderState shader path Nothing
      ProgramC $ modify (Map.insert program shaders)
      k program
    L (Use p     k) -> do
      shaders <- maybe (error "no state found for program") id <$> ProgramC (gets (Map.lookup p))
      let prevTimes = map time shaders
      times <- traverse (fmap Just . sendM . getModificationTime . path) shaders
      when (times /= prevTimes) $ do
        shaders <- for (zip times shaders) $ \ (newTime, ShaderState shader path oldTime) -> do
          when (newTime /= oldTime) $ do
            source <- sendM $ readFile path
            compile source shader
          pure (ShaderState shader path newTime)
        ProgramC (modify (Map.insert p shaders))
        GL.link (map shader shaders) p
      GL.useProgram p
      k
    L (Set p v a k) -> setUniformValue p v a >> k
    R other         -> ProgramC (send (handleCoercible other))

data ShaderState = ShaderState
  { shader :: !Shader
  , path   :: !FilePath
  , time   :: !(Maybe UTCTime)
  }
