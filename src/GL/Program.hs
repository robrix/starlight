module GL.Program
( Program(..)
, useProgram
, withBuiltProgram
, checkProgram
) where

import qualified Control.Exception as E
import Control.Monad.IO.Class
import Data.Foldable (for_)
import GHC.Stack
import GL.Error
import GL.Shader
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Program = Program { unProgram :: GLuint }
  deriving Show

useProgram :: MonadIO m => Program -> m ()
useProgram = glUseProgram . unProgram

withProgram :: (Program -> IO a) -> IO a
withProgram = E.bracket
  (Program <$> glCreateProgram)
  (glDeleteProgram . unProgram)

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
