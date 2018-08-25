{-# LANGUAGE LambdaCase #-}
module GL.Error where

import qualified Control.Exception as E
import Control.Monad
import qualified Foreign.C.String as C
import qualified Foreign.Marshal.Alloc as A
import Foreign.Ptr
import qualified Foreign.Storable as S
import GHC.Stack
import Graphics.GL.Core41
import Graphics.GL.Types

data GLError
  = InvalidEnum
  | InvalidValue
  | InvalidOperation
  | InvalidFramebufferOperation
  | OutOfMemory
  | Source String String
  | Other String

instance Show GLError where
  showsPrec _ e = case e of
    InvalidEnum -> showString "GL_INVALID_ENUM"
    InvalidValue -> showString "GL_INVALID_VALUE"
    InvalidOperation -> showString "GL_INVALID_OPERATION"
    InvalidFramebufferOperation -> showString "GL_INVALID_FRAMEBUFFER_OPERATION"
    OutOfMemory -> showString "GL_OUT_OF_MEMORY"
    Source s t -> showString s . showChar '\n' . showString t
    Other s -> showString s


data GLException = GLException GLError CallStack

instance Show GLException where
  showsPrec p (GLException e s) = showString "GLException " . showsPrec p e . showChar '\n' . showString (prettyCallStack s)

instance E.Exception GLException


checkStatus :: HasCallStack
            => (GLenum -> GLuint -> Ptr GLint -> IO ())
            -> (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
            -> (String -> GLError)
            -> GLenum
            -> GLuint
            -> IO GLuint
checkStatus get getLog error status object = withFrozenCallStack $ do
  success <- A.alloca $ \ p -> do
    get object status p
    S.peek p
  when (success == GL_FALSE) $ do
    l <- A.alloca $ \ p -> do
      get object GL_INFO_LOG_LENGTH p
      S.peek p
    log <- A.allocaBytes (fromIntegral l) $ \ bytes -> do
      getLog object l nullPtr bytes
      C.peekCString bytes
    E.throw $ GLException (error log) callStack
  pure object

checkGLError :: HasCallStack => IO ()
checkGLError = withFrozenCallStack $ glGetError >>= throwGLError

checkingGLError :: HasCallStack => IO a -> IO a
checkingGLError action = withFrozenCallStack $ do
  result <- action
  checkGLError
  pure result

throwGLError :: HasCallStack => GLenum -> IO ()
throwGLError = \case
  GL_NO_ERROR -> pure ()
  GL_INVALID_ENUM -> E.throw $ GLException InvalidEnum callStack
  GL_INVALID_VALUE -> E.throw $ GLException InvalidValue callStack
  GL_INVALID_OPERATION -> E.throw $ GLException InvalidOperation callStack
  GL_INVALID_FRAMEBUFFER_OPERATION -> E.throw $ GLException InvalidFramebufferOperation callStack
  GL_OUT_OF_MEMORY -> E.throw $ GLException OutOfMemory callStack
  _ -> E.throw $ GLException (Other "Unknown") callStack
