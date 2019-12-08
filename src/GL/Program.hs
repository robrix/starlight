module GL.Program
( Program(..)
, useProgram
, withProgram
, withLinkedProgram
, link
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
  deriving (Eq, Ord, Show)

useProgram :: Has (Lift IO) sig m => Program -> m ()
useProgram = runLiftIO . glUseProgram . unProgram

withProgram :: Has (Lift IO) sig m => (Program -> m a) -> m a
withProgram = E.bracket
  (runLiftIO (Program <$> glCreateProgram))
  (runLiftIO . glDeleteProgram . unProgram)

withLinkedProgram :: (Has (Lift IO) sig m, HasCallStack) => [Shader] -> (Program -> m a) -> m a
withLinkedProgram shaders body = withProgram $ \ program -> do
  link shaders program
  body program

link :: (Has (Lift IO) sig m, HasCallStack) => [Shader] -> Program -> m ()
link shaders (Program program) = runLiftIO $ do
  for_ shaders (glAttachShader program . unShader)
  glLinkProgram program
  for_ shaders (glDetachShader program . unShader)
  checkProgram (Program program)

withBuiltProgram :: (Has (Lift IO) sig m, HasCallStack) => [(ShaderType, String)] -> (Program -> m a) -> m a
withBuiltProgram sources body = withCompiledShaders sources (`withLinkedProgram` body)


checkProgram :: (Has (Lift IO) sig m, HasCallStack) => Program -> m ()
checkProgram = runLiftIO . checkStatus glGetProgramiv glGetProgramInfoLog Other GL_LINK_STATUS . unProgram
