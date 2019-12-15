{-# LANGUAGE DataKinds, FunctionalDependencies, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, ScopedTypeVariables #-}
module GL.Texture
( Texture(..)
, Type(..)
, InternalFormat(..)
, PixelFormat(..)
, KnownType(..)
, FilterType(..)
, Filter(..)
, WrapCoord(..)
, Wrap(..)
, setParameter
, Parameter
) where

import Control.Monad.IO.Class.Lift
import Data.Coerce
import Data.Proxy
import Foreign.Storable
import GL.Enum as GL
import GL.Error
import GL.Object
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Texture (ty :: Type) = Texture { unTexture :: GLuint }
  deriving (Storable)

instance Object (Texture ty) where
  gen n = runLiftIO . glGenTextures n . coerce
  delete n = runLiftIO . glDeleteTextures n . coerce

instance KnownType ty => Bind (Texture ty) where
  bind = checkingGLError . runLiftIO . glBindTexture (glEnum (typeVal (Proxy :: Proxy ty))) . maybe 0 unTexture


data Type
  = Texture2D
  deriving (Eq, Ord, Show)

class KnownType (ty :: Type) where
  typeVal :: proxy ty -> Type

instance KnownType 'Texture2D where
  typeVal _ = Texture2D

instance GL.Enum Type where
  glEnum = \case
    Texture2D -> GL_TEXTURE_2D


data InternalFormat
  = RGBA8

instance GL.Enum InternalFormat where
  glEnum = \case
    RGBA8 -> GL_RGBA8

data PixelFormat
  = RGBA

instance GL.Enum PixelFormat where
  glEnum = \case
    RGBA -> GL_RGBA


data FilterType = MinFilter | MagFilter

instance GL.Enum FilterType where
  glEnum = \case
    MinFilter -> GL_TEXTURE_MIN_FILTER
    MagFilter -> GL_TEXTURE_MAG_FILTER


data Filter = Nearest | Linear

instance GL.Enum Filter where
  glEnum = \case
    Nearest -> GL_NEAREST
    Linear  -> GL_LINEAR


data WrapCoord = WrapR | WrapS | WrapT

instance GL.Enum WrapCoord where
  glEnum = \case
    WrapR -> GL_TEXTURE_WRAP_R
    WrapS -> GL_TEXTURE_WRAP_S
    WrapT -> GL_TEXTURE_WRAP_T


data Wrap
  = Repeat
  | MirroredRepeat
  | ClampToEdge
  | ClampToBorder

instance GL.Enum Wrap where
  glEnum = \case
    Repeat         -> GL_REPEAT
    MirroredRepeat -> GL_MIRRORED_REPEAT
    ClampToEdge    -> GL_CLAMP_TO_EDGE
    ClampToBorder  -> GL_CLAMP_TO_BORDER


setParameter :: (Parameter val param, Has (Lift IO) sig m) => Type -> param -> val -> m ()
setParameter target param = checkingGLError . runLiftIO . glTexParameteri (glEnum target) (glEnum param) . fromIntegral . glEnum

class (GL.Enum param, GL.Enum val) => Parameter val param | param -> val

instance Parameter Filter FilterType
instance Parameter Wrap WrapCoord
