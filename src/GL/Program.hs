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

withLinkedProgram :: HasCallStack => [Shader] -> (Program -> IO a) -> IO a
withLinkedProgram shaders body = withProgram $ \ (Program program) -> do
  for_ shaders (glAttachShader program . unShader)
  glLinkProgram program
  for_ shaders (glDetachShader program . unShader)
  p <- checkProgram (Program program)
  body p


withBuiltProgram :: HasCallStack => [(ShaderType, String)] -> (Program -> IO a) -> IO a
withBuiltProgram sources body = withCompiledShaders sources (`withLinkedProgram` body)


checkProgram :: HasCallStack => Program -> IO Program
checkProgram = fmap Program . checkStatus glGetProgramiv glGetProgramInfoLog Other GL_LINK_STATUS . unProgram
