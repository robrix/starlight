{-# LANGUAGE ExplicitForAll, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module GL.Carrier.Program.Live
( -- * Program carrier
  runProgram
, ProgramC(..)
  -- * Program effect
, module GL.Effect.Program
) where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Monad (when)
import Control.Monad.IO.Class.Lift
import Control.Monad.Trans.Class
import Data.Time.Clock (UTCTime)
import Data.Traversable (for)
import Foreign.Marshal.Utils (withMany)
import GL.Effect.Program
import GL.Shader
import qualified GL.Program as GL
import GL.Uniform
import System.Directory

runProgram :: forall name sig m a . Has (Lift IO) sig m => [(ShaderType, FilePath)] -> ProgramC name m a -> m a
runProgram shaders (ProgramC m) = GL.withProgram $ \ program ->
  withMany withShader (map fst shaders) $ \ shaders' ->
    runReader program (runReader (zipWith ((,) . snd) shaders shaders') (evalState (Nothing <$ shaders) m))

newtype ProgramC name m a = ProgramC (StateC [Maybe UTCTime] (ReaderC [(FilePath, Shader)] (ReaderC GL.Program m)) a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance MonadTrans (ProgramC name) where
  lift = ProgramC . lift . lift . lift

instance (Has (Lift IO) sig m, Effect sig) => Algebra (Program name :+: sig) (ProgramC name m) where
  alg = \case
    L (Use k)     -> do
      (program, shaders, prevTimes) <- ProgramC ((,,) <$> ask <*> ask <*> get)
      times <- traverse (fmap Just . sendM . getModificationTime . fst) (shaders :: [(FilePath, Shader)])
      when (times /= prevTimes) $ do
        shaders <- for shaders $ \ (path, shader) -> do
          source <- sendM $ readFile path
          shader <$ compile source shader
        GL.link shaders program
      GL.useProgram program
      k
    L (Set v a k) -> ProgramC ask >>= \ p -> setUniformValue p v a >> k
    R other       -> ProgramC (send (handleCoercible other))
