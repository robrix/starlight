{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GL.Framebuffer
( Framebuffer(..)
, Attachment(..)
, attachTexture
, Bind(..)
) where

import           Control.Monad (unless)
import           Control.Monad.IO.Class.Lift
import           Data.Coerce
import           Data.Proxy
import           Foreign.Storable
import           GL.Enum as GL
import           GL.Error
import           GL.Object
import qualified GL.Texture as GL
import           Graphics.GL.Core41
import           Graphics.GL.Types

newtype Framebuffer = Framebuffer { unFramebuffer :: GLuint }
  deriving (Storable)

instance Object Framebuffer where
  gen n = runLiftIO . glGenFramebuffers n . coerce
  delete n = runLiftIO . glDeleteFramebuffers n . coerce

instance Bind Framebuffer where
  bind = checkingGLError . runLiftIO . glBindFramebuffer GL_FRAMEBUFFER . maybe 0 unFramebuffer


data Attachment
  = Colour Int

instance GL.Enum Attachment where
  glEnum = \case
    Colour n -> GL_COLOR_ATTACHMENT0 + fromIntegral n


attachTexture :: forall ty sig m . Has (Lift IO) sig m => GL.KnownType ty => Attachment -> GL.Texture ty -> m ()
attachTexture attachment (GL.Texture texture) = runLiftIO $ do
  checkingGLError $ glFramebufferTexture2D GL_FRAMEBUFFER (glEnum attachment) (glEnum (GL.typeVal (Proxy :: Proxy ty))) texture 0
  status <- glCheckFramebufferStatus GL_FRAMEBUFFER
  unless (status == GL_FRAMEBUFFER_COMPLETE) (throwGLError status)
