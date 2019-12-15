{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GL.Framebuffer
( Framebuffer(..)
, Attachment(..)
, Bind(..)
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
  gen n = runLiftIO . glGenFramebuffers n . coerce
  delete n = runLiftIO . glDeleteFramebuffers n . coerce

instance Bind Framebuffer where
  bind = checkingGLError . runLiftIO . glBindFramebuffer GL_FRAMEBUFFER . maybe 0 unFramebuffer


data Attachment
  = Colour Int
