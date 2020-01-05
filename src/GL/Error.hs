{-# LANGUAGE LambdaCase #-}
module GL.Error
( GLError(..)
, GLException(..)
, checkStatus
, throwGLError
) where

import qualified Control.Exception.Lift as E
import           Control.Monad
import           Control.Monad.IO.Class.Lift
import qualified Foreign.C.String as C
import qualified Foreign.Marshal.Alloc.Lift as A
import           Foreign.Ptr
import qualified Foreign.Storable as S
import           GHC.Stack
import           Graphics.GL.Core41
import           Graphics.GL.Types

data GLError
  = InvalidEnum
  | InvalidValue
  | InvalidOperation
  | InvalidFramebufferOperation
  | OutOfMemory
  | FramebufferIncompleteAttachment
  -- | FramebufferIncompleteDimensions
  | FramebufferIncompleteMissingAttachment
  | FramebufferUnsupported
  | Source String String
  | Other String

instance Show GLError where
  showsPrec _ = \case
    InvalidEnum                            -> showString "GL_INVALID_ENUM"
    InvalidValue                           -> showString "GL_INVALID_VALUE"
    InvalidOperation                       -> showString "GL_INVALID_OPERATION"
    InvalidFramebufferOperation            -> showString "GL_INVALID_FRAMEBUFFER_OPERATION"
    OutOfMemory                            -> showString "GL_OUT_OF_MEMORY"
    FramebufferIncompleteAttachment        -> showString "GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT"
    -- FramebufferIncompleteDimensions        -> showString "GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS"
    FramebufferIncompleteMissingAttachment -> showString "GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT"
    FramebufferUnsupported                 -> showString "GL_FRAMEBUFFER_UNSUPPORTED"
    Source s t                             -> showString s . showChar '\n' . showString t
    Other s                                -> showString s


data GLException = GLException GLError CallStack

instance Show GLException where
  showsPrec p (GLException e s) = showString "GLException " . showsPrec p e . showChar '\n' . showString (prettyCallStack s)

instance E.Exception GLException


checkStatus :: (Has (Lift IO) sig m, HasCallStack)
            => (GLenum -> GLuint -> Ptr GLint -> m ())
            -> (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> m ())
            -> (String -> GLError)
            -> GLenum
            -> GLuint
            -> m ()
checkStatus get getLog error status object = withFrozenCallStack $ do
  success <- A.alloca $ \ p -> do
    get object status p
    sendM (S.peek p)
  when (success == GL_FALSE) $ do
    l <- A.alloca $ \ p -> do
      get object GL_INFO_LOG_LENGTH p
      sendM (S.peek p)
    log <- A.allocaBytes (fromIntegral l) $ \ bytes -> do
      getLog object l nullPtr bytes
      sendM (C.peekCString bytes)
    E.throwIO $ GLException (error log) callStack

throwGLError :: (Has (Lift IO) sig m, HasCallStack) => GLenum -> m ()
throwGLError = \case
  GL_NO_ERROR                                  -> pure ()
  GL_INVALID_ENUM                              -> E.throwIO $ GLException InvalidEnum                            callStack
  GL_INVALID_VALUE                             -> E.throwIO $ GLException InvalidValue                           callStack
  GL_INVALID_OPERATION                         -> E.throwIO $ GLException InvalidOperation                       callStack
  GL_INVALID_FRAMEBUFFER_OPERATION             -> E.throwIO $ GLException InvalidFramebufferOperation            callStack
  GL_OUT_OF_MEMORY                             -> E.throwIO $ GLException OutOfMemory                            callStack
  GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT         -> E.throwIO $ GLException FramebufferIncompleteAttachment        callStack
  -- GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS         -> E.throwIO $ GLException FramebufferIncompleteDimensions        callStack
  GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT -> E.throwIO $ GLException FramebufferIncompleteMissingAttachment callStack
  GL_FRAMEBUFFER_UNSUPPORTED                   -> E.throwIO $ GLException FramebufferUnsupported                 callStack
  _                                            -> E.throwIO $ GLException (Other "Unknown")                      callStack
