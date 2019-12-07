{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, KindSignatures, ScopedTypeVariables #-}
module GL.Texture
( Texture(..)
, Type(..)
, KnownType(..)
, typeToGLEnum
, Filter(..)
, filterToGLEnum
, setMagFilter
, setMinFilter
) where

import Control.Monad.IO.Class.Lift
import Data.Coerce
import Data.Proxy
import Foreign.Storable
import GL.Error
import GL.Object
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Texture (ty :: Type) = Texture { unTexture :: GLuint }
  deriving (Storable)

instance Object (Texture ty) where
  gen n = glGenTextures n . coerce
  delete n = glDeleteTextures n . coerce

instance KnownType ty => Bind (Texture ty) where
  nullObject = Texture 0
  bindObject = checkingGLError . runLiftIO . glBindTexture (typeToGLEnum (typeVal (Proxy :: Proxy ty))) . unTexture


data Type
  = Texture2D
  deriving (Eq, Ord, Show)

class KnownType (ty :: Type) where
  typeVal :: proxy ty -> Type

instance KnownType 'Texture2D where
  typeVal _ = Texture2D

typeToGLEnum :: Type -> GLenum
typeToGLEnum Texture2D = GL_TEXTURE_2D


data Filter = Nearest | Linear

filterToGLEnum :: Filter -> GLenum
filterToGLEnum Nearest = GL_NEAREST
filterToGLEnum Linear = GL_LINEAR

setMagFilter :: Has (Lift IO) sig m => Type -> Filter -> m ()
setMagFilter target = checkingGLError . runLiftIO . glTexParameteri (typeToGLEnum target) GL_TEXTURE_MAG_FILTER . fromIntegral . filterToGLEnum

setMinFilter :: Has (Lift IO) sig m => Type -> Filter -> m ()
setMinFilter target = checkingGLError . runLiftIO . glTexParameteri (typeToGLEnum target) GL_TEXTURE_MIN_FILTER . fromIntegral . filterToGLEnum
