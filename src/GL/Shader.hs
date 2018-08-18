module GL.Shader where

import qualified Control.Exception as E
import qualified Foreign.C.String as C
import qualified Foreign.Marshal.Alloc as A
import Foreign.Ptr
import qualified Foreign.Storable as S
import GHC.Stack
import GL.Error
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Shader = Shader { unShader :: GLuint }

data ShaderType = Vertex | Fragment


withShader :: GLenum -> (Shader -> IO a) -> IO a
withShader shaderType = E.bracket
  (Shader <$> glCreateShader shaderType)
  (glDeleteShader . unShader)

withCompiledShader :: HasCallStack => GLenum -> String -> (Shader -> IO a) -> IO a
withCompiledShader shaderType source body = withShader shaderType $ \ (Shader shader) -> do
    C.withCString source $ \ source ->
      A.alloca $ \ p -> do
        S.poke p source
        glShaderSource shader 1 p nullPtr
    glCompileShader shader
    s <- checkShader source (Shader shader)
    body s

withCompiledShaders :: HasCallStack => [(GLenum, String)] -> ([Shader] -> IO a) -> IO a
withCompiledShaders sources body = go sources []
  where go [] shaders = body shaders
        go ((t, source):xs) shaders = withCompiledShader t source (\ shader -> go xs (shader : shaders))

checkShader :: HasCallStack => String -> Shader -> IO Shader
checkShader source = withFrozenCallStack $ fmap Shader . checkStatus glGetShaderiv glGetShaderInfoLog (Source source) GL_COMPILE_STATUS . unShader
