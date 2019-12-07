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
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Time.Clock (UTCTime)
import GL.Effect.Program
import GL.Shader
import qualified GL.Program as GL
import GL.Uniform

runProgram :: forall name sig m a . Has (Lift IO) sig m => [(ShaderType, FilePath)] -> ProgramC name m a -> m a
runProgram shaders (ProgramC m) = do
  shaders' <- traverse (traverse (sendM . readFile)) shaders
  GL.withBuiltProgram shaders' $ \ program ->
    runReader program (evalState (map (\ (_, p) -> (p, Nothing)) shaders) m)

newtype ProgramC name m a = ProgramC (StateC [(FilePath, Maybe UTCTime)] (ReaderC GL.Program m) a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance MonadTrans (ProgramC name) where
  lift = ProgramC . lift . lift

instance (Has (Lift IO) sig m, Effect sig) => Algebra (Program name :+: sig) (ProgramC name m) where
  alg = \case
    L (Use k)     -> ProgramC ask >>= GL.useProgram >> k
    L (Set v a k) -> ProgramC ask >>= \ p -> setUniformValue p v a >> k
    R other       -> ProgramC (send (handleCoercible other))
