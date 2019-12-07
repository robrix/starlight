{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, KindSignatures #-}
module GL.Buffer
( Buffer(..)
, Type(..)
, KnownType(..)
) where

import Control.Monad.IO.Class.Lift
import Data.Coerce
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

instance Bind (Buffer ty n) where
  nullObject = Buffer 0
  bindObject = checkingGLError . runLiftIO . glBindBuffer GL_ARRAY_BUFFER . unBuffer


data Type
  = Array

class KnownType (ty :: Type) where
  typeVal :: proxy ty -> Type
