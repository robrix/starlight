{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GL.Framebuffer
( Framebuffer(..)
, Bind(..)
, bindFramebuffer
) where

import Control.Monad.IO.Class.Lift
import Data.Coerce
import Foreign.Storable
import GL.Error
import GL.Object
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Framebuffer = Framebuffer { unFramebuffer :: GLuint }
  deriving (Storable)

instance Object Framebuffer where
  gen n = glGenFramebuffers n . coerce
  delete n = glDeleteFramebuffers n . coerce

instance Bind Framebuffer where
  bind (Framebuffer t) m = runLiftIO $ do
    checkingGLError $ glBindFramebuffer GL_FRAMEBUFFER t
    a <- LiftIO m
    a <$ checkingGLError (glBindFramebuffer GL_FRAMEBUFFER 0)


bindFramebuffer :: Has (Lift IO) sig m => Maybe Framebuffer -> m ()
bindFramebuffer = checkingGLError . runLiftIO . glBindFramebuffer GL_FRAMEBUFFER . maybe 0 unFramebuffer
