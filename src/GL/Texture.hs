{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, KindSignatures #-}
module GL.Texture
( Texture(..)
, Type(..)
, KnownType(..)
, targetToGLEnum
, bindTexture
, Filter(..)
, filterToGLEnum
, setMagFilter
, setMinFilter
) where

import Control.Monad.IO.Class.Lift
import Data.Coerce
import Foreign.Storable
import GHC.Stack
import GL.Error
import GL.Object
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Texture (ty :: Type) = Texture { unTexture :: GLuint }
  deriving (Storable)

instance Object (Texture ty) where
  gen n = glGenTextures n . coerce
  delete n = glDeleteTextures n . coerce


data Type = Texture2D

class KnownType (ty :: Type) where
  typeVal :: proxy ty -> Type

targetToGLEnum :: Type -> GLenum
targetToGLEnum Texture2D = GL_TEXTURE_2D


bindTexture :: (Has (Lift IO) sig m, HasCallStack) => Type -> Maybe (Texture ty) -> m ()
bindTexture target = checkingGLError . runLiftIO . glBindTexture (targetToGLEnum target) . maybe 0 unTexture


data Filter = Nearest | Linear

filterToGLEnum :: Filter -> GLenum
filterToGLEnum Nearest = GL_NEAREST
filterToGLEnum Linear = GL_LINEAR

setMagFilter :: Has (Lift IO) sig m => Type -> Filter -> m ()
setMagFilter target = checkingGLError . runLiftIO . glTexParameteri (targetToGLEnum target) GL_TEXTURE_MAG_FILTER . fromIntegral . filterToGLEnum

setMinFilter :: Has (Lift IO) sig m => Type -> Filter -> m ()
setMinFilter target = checkingGLError . runLiftIO . glTexParameteri (targetToGLEnum target) GL_TEXTURE_MIN_FILTER . fromIntegral . filterToGLEnum
