{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, KindSignatures, ScopedTypeVariables #-}
module GL.Buffer
( Buffer(..)
, Type(..)
, KnownType(..)
, typeToGLEnum
) where

import Control.Monad.IO.Class.Lift
import Data.Coerce
import Data.Proxy
import Foreign.Storable
import GL.Error
import GL.Object
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Buffer (ty :: Type) n = Buffer { unBuffer :: GLuint }
  deriving (Storable)

instance Object (Buffer ty n) where
  gen n = glGenBuffers n . coerce
  delete n = glDeleteBuffers n . coerce

instance KnownType ty => Bind (Buffer ty n) where
  nullObject = Buffer 0
  bindObject = checkingGLError . runLiftIO . glBindBuffer (typeToGLEnum (typeVal (Proxy :: Proxy ty))) . unBuffer


data Type
  = Array
  deriving (Eq, Ord, Show)

class KnownType (ty :: Type) where
  typeVal :: proxy ty -> Type

instance KnownType 'Array where
  typeVal _ = Array

typeToGLEnum :: Type -> GLenum
typeToGLEnum Array = GL_ARRAY_BUFFER
