{-# LANGUAGE ExplicitForAll, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module GL.Carrier.Program
( -- * Program carrier
  runProgram
, ProgramC(..)
  -- * Program effect
, module GL.Effect.Program
) where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Monad.IO.Class
import GL.Effect.Program
import GL.Shader
import qualified GL.Program as GL

runProgram :: forall name sig m a . Has (Lift IO) sig m => [(ShaderType, FilePath)] -> ProgramC name m a -> m a
runProgram shaders (ProgramC m) = do
  shaders <- traverse (traverse (sendM . readFile)) shaders
  GL.withBuiltProgram shaders $ \ program ->
    runReader program m

newtype ProgramC name m a = ProgramC (ReaderC GL.Program m a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance Has (Lift IO) sig m => Algebra (Program name :+: sig) (ProgramC name m) where
  alg = \case
    L (Use k) -> ProgramC ask >>= GL.useProgram >> k
    R other   -> ProgramC (send (handleCoercible other))
