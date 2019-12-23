{-# LANGUAGE DataKinds, FlexibleContexts, KindSignatures, LambdaCase #-}
module GL.Shader
( Shader(..)
, Type(..)
, KnownType(..)
, createShader
, compile
, checkShader
) where

import Control.Effect.Finally
import Control.Monad.IO.Class.Lift
import qualified Foreign.C.String.Lift as C
import qualified Foreign.Marshal.Utils.Lift as U
import Foreign.Ptr
import GHC.Stack
import qualified GL.Enum as GL
import GL.Error
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Shader = Shader { unShader :: GLuint }

data Type = Vertex | Fragment

instance GL.Enum Type where
  glEnum = \case
    Vertex   -> GL_VERTEX_SHADER
    Fragment -> GL_FRAGMENT_SHADER


class KnownType (k :: Type) where
  typeVal :: proxy k -> Type

instance KnownType 'Vertex where
  typeVal _ = Vertex

instance KnownType 'Fragment where
  typeVal _ = Fragment


createShader :: (Has Finally sig m, Has (Lift IO) sig m) => Type -> m Shader
createShader type' = do
  shader <- runLiftIO (glCreateShader (GL.glEnum type'))
  Shader shader <$ onExit (runLiftIO (glDeleteShader shader))

compile :: (Has (Lift IO) sig m, HasCallStack) => String -> Shader -> m ()
compile source (Shader shader) = runLiftIO $ do
  C.withCString source $ \ source ->
    U.with source $ \ p ->
      glShaderSource shader 1 p nullPtr
  glCompileShader shader
  checkShader source (Shader shader)

checkShader :: (Has (Lift IO) sig m, HasCallStack) => String -> Shader -> m ()
checkShader source = withFrozenCallStack $ runLiftIO . checkStatus glGetShaderiv glGetShaderInfoLog (Source source) GL_COMPILE_STATUS . unShader
