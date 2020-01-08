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
import           Data.Proxy
import           GHC.Stack
import           GL.Effect.Check
import           GL.Enum as GL
import           GL.Error
import           GL.Object
import qualified GL.Texture as GL
import           Graphics.GL.Core41
import           Graphics.GL.Types

newtype Framebuffer = Framebuffer { unFramebuffer :: GLuint }

instance Object Framebuffer where
  gen = defaultGenWith glGenFramebuffers Framebuffer
  delete = defaultDeleteWith glDeleteFramebuffers unFramebuffer

instance Bind Framebuffer where
  bind = checking . runLiftIO . glBindFramebuffer GL_FRAMEBUFFER . maybe 0 unFramebuffer


data Attachment
  = Colour Int

instance GL.Enum Attachment where
  glEnum = \case
    Colour n -> GL_COLOR_ATTACHMENT0 + fromIntegral n


attachTexture :: forall ty sig m . (HasCallStack, Has Check sig m, Has (Lift IO) sig m) => GL.KnownType ty => Attachment -> GL.Texture ty -> m ()
attachTexture attachment (GL.Texture texture) = runLiftIO $ do
  checking $ glFramebufferTexture2D GL_FRAMEBUFFER (glEnum attachment) (glEnum (GL.typeVal (Proxy :: Proxy ty))) texture 0
  status <- glCheckFramebufferStatus GL_FRAMEBUFFER
  unless (status == GL_FRAMEBUFFER_COMPLETE) (throwGLError status)
