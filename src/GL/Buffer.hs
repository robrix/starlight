{-# LANGUAGE DataKinds, FlexibleContexts, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, ScopedTypeVariables, TypeApplications #-}
module GL.Buffer
( Buffer(..)
, realloc
, copyPtr
, copy
, Type(..)
, KnownType(..)
, Update(..)
, Usage(..)
, hintToGLEnum
, Range(..)
) where

import Control.Monad.IO.Class.Lift
import Data.Coerce
import Data.Proxy
import qualified Foreign.Marshal.Array.Lift as A
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable as S
import GL.Enum as GL
import GL.Error
import GL.Object
import GL.Range
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Buffer (ty :: Type) v = Buffer { unBuffer :: GLuint }
  deriving (Storable)

instance Object (Buffer ty v) where
  gen n = runLiftIO . glGenBuffers n . coerce
  delete n = runLiftIO . glDeleteBuffers n . coerce

instance KnownType ty => Bind (Buffer ty v) where
  bind = checkingGLError . runLiftIO . glBindBuffer (glEnum (typeVal (Proxy :: Proxy ty))) . maybe 0 unBuffer

realloc :: forall ty v m buffer sig . (KnownType ty, S.Storable v, Has (Lift IO) sig m) => buffer ty v -> Int -> Update -> Usage -> m ()
realloc _ n update usage = runLiftIO (glBufferData (glEnum (typeVal (Proxy @ty))) (fromIntegral (n * S.sizeOf @v undefined)) nullPtr (hintToGLEnum update usage))

copyPtr :: forall ty a m buffer sig . (KnownType ty, Has (Lift IO) sig m) => buffer ty a -> Range -> Ptr a -> m ()
copyPtr _ (Range offset size) = checkingGLError . runLiftIO . glBufferSubData (glEnum (typeVal (Proxy @ty))) (fromIntegral offset) (fromIntegral size) . castPtr

copy :: forall ty v m buffer sig . (KnownType ty, S.Storable v, Has (Lift IO) sig m) => buffer ty v -> Int -> [v] -> m ()
copy buffer offset vertices = A.withArray vertices $ copyPtr buffer (Range (offset * vsize) size) where
  size = length vertices * vsize
  vsize = S.sizeOf @v undefined


data Type
  = Array
  deriving (Eq, Ord, Show)

class KnownType (ty :: Type) where
  typeVal :: proxy ty -> Type

instance KnownType 'Array where
  typeVal _ = Array

instance GL.Enum Type where
  glEnum = \case
    Array -> GL_ARRAY_BUFFER


data Update
  = Static
  | Dynamic
  | Stream
  deriving (Eq, Ord, Show)

data Usage
  = Draw
  | Read
  | Copy
  deriving (Eq, Ord, Show)

hintToGLEnum :: Update -> Usage -> GLenum
hintToGLEnum = curry $ \case
  (Static,  Draw) -> GL_STATIC_DRAW
  (Static,  Read) -> GL_STATIC_READ
  (Static,  Copy) -> GL_STATIC_COPY
  (Dynamic, Draw) -> GL_DYNAMIC_DRAW
  (Dynamic, Read) -> GL_DYNAMIC_READ
  (Dynamic, Copy) -> GL_DYNAMIC_COPY
  (Stream,  Draw) -> GL_STREAM_DRAW
  (Stream,  Read) -> GL_STREAM_READ
  (Stream,  Copy) -> GL_STREAM_COPY
