module GL.Program
( Program(..)
, useProgram
, withBuiltProgram
, checkProgram
) where

import qualified Control.Exception.Lift as E
import Control.Monad.IO.Class.Lift
import Data.Foldable (for_)
import GHC.Stack
import GL.Error
import GL.Shader
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Program = Program { unProgram :: GLuint }
  deriving Show

useProgram :: Has (Lift IO) sig m => Program -> m ()
useProgram = runLifting . glUseProgram . unProgram

withProgram :: Has (Lift IO) sig m => (Program -> m a) -> m a
withProgram = E.bracket
  (runLifting (Program <$> glCreateProgram))
  (runLifting . glDeleteProgram . unProgram)

withLinkedProgram :: (Has (Lift IO) sig m, HasCallStack) => [Shader] -> (Program -> m a) -> m a
withLinkedProgram shaders body = withProgram $ \ (Program program) -> runLifting $ do
  for_ shaders (glAttachShader program . unShader)
  glLinkProgram program
  for_ shaders (glDetachShader program . unShader)
  p <- checkProgram (Program program)
  Lifting (body p)


withBuiltProgram :: HasCallStack => [(ShaderType, String)] -> (Program -> IO a) -> IO a
withBuiltProgram sources body = withCompiledShaders sources (`withLinkedProgram` body)


checkProgram :: (Has (Lift IO) sig m, HasCallStack) => Program -> m Program
checkProgram = runLifting . fmap Program . checkStatus glGetProgramiv glGetProgramInfoLog Other GL_LINK_STATUS . unProgram
