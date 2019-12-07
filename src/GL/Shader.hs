{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module GL.Shader
( Shader(..)
, ShaderType(..)
, withCompiledShaders
, checkShader
) where

import qualified Control.Exception.Lift as E
import Control.Monad.IO.Class.Lift
import qualified Foreign.C.String.Lift as C
import qualified Foreign.Marshal.Alloc.Lift as A
import Foreign.Ptr
import qualified Foreign.Storable as S
import GHC.Stack
import GL.Error
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Shader = Shader { unShader :: GLuint }

data ShaderType = Vertex | Fragment

toGLEnum :: ShaderType -> GLenum
toGLEnum Vertex   = GL_VERTEX_SHADER
toGLEnum Fragment = GL_FRAGMENT_SHADER


withShader :: Has (Lift IO) sig m => ShaderType -> (Shader -> m a) -> m a
withShader shaderType = E.bracket
  (runLifting (Shader <$> glCreateShader (toGLEnum shaderType)))
  (runLifting . glDeleteShader . unShader)

withCompiledShader :: (Has (Lift IO) sig m, HasCallStack) => ShaderType -> String -> (Shader -> m a) -> m a
withCompiledShader shaderType source body = withShader shaderType $ \ (Shader shader) -> runLifting $ do
  C.withCString source $ \ source ->
    A.alloca $ \ p -> do
      sendM (S.poke p source)
      glShaderSource shader 1 p nullPtr
  glCompileShader shader
  s <- checkShader source (Shader shader)
  Lifting (body s)

withCompiledShaders :: (Has (Lift IO) sig m, HasCallStack) => [(ShaderType, String)] -> ([Shader] -> m a) -> m a
withCompiledShaders sources body = go [] sources where
  go shaders = \case
    []             -> body shaders
    (t, source):xs -> withCompiledShader t source (\ shader -> go (shader : shaders) xs)

checkShader :: (Has (Lift IO) sig m, HasCallStack) => String -> Shader -> m Shader
checkShader source = withFrozenCallStack $ runLifting . fmap Shader . checkStatus glGetShaderiv glGetShaderInfoLog (Source source) GL_COMPILE_STATUS . unShader
