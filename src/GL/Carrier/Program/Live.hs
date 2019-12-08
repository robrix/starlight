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
import qualified Data.IntMap as IntMap
import Data.Time.Clock (UTCTime)
import Data.Traversable (for)
import GL.Effect.Program
import GL.Shader
import qualified GL.Program as GL
import System.Directory

runProgram :: Has (Lift IO) sig m => ProgramC m a -> m a
runProgram (ProgramC m) = evalState IntMap.empty m

newtype ProgramC m a = ProgramC (StateC (IntMap.IntMap [ShaderState]) m a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance (Has Finally sig m, Has (Lift IO) sig m, Effect sig) => Algebra (Program :+: sig) (ProgramC m) where
  alg = \case
    L (Build s k) -> do
      program <- GL.createProgram
      shaders <- for s $ \ (type', path) -> do
        shader <- createShader type'
        pure $! ShaderState shader path Nothing
      ProgramC $ modify (insert program shaders)
      k program
    L (Use p   k) -> do
      shaders <- maybe (error "no state found for program") id <$> ProgramC (gets (lookup p))
      let prevTimes = map time shaders
      times <- traverse (fmap Just . sendM . getModificationTime . path) shaders
      when (times /= prevTimes) $ do
        shaders <- for (zip times shaders) $ \ (newTime, ShaderState shader path oldTime) -> do
          when (newTime /= oldTime) $ do
            source <- sendM $ readFile path
            compile source shader
          pure (ShaderState shader path newTime)
        ProgramC (modify (insert p shaders))
        GL.link (map shader shaders) p
      GL.useProgram p
      k
    L (Set p v k) -> GL.setUniformValue p v >> k
    R other       -> ProgramC (send (handleCoercible other))
    where
      lookup = IntMap.lookup . fromIntegral . GL.unProgram
      insert p v = IntMap.insert (fromIntegral (GL.unProgram p)) v

data ShaderState = ShaderState
  { shader :: !Shader
  , path   :: !FilePath
  , time   :: !(Maybe UTCTime)
  }
