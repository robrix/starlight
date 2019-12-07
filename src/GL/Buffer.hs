{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, ScopedTypeVariables, TypeApplications #-}
module GL.Buffer
( Buffer(..)
, realloc
, copy
, Type(..)
, KnownType(..)
, typeToGLEnum
, Update(..)
, Usage(..)
, hintToGLEnum
, Range(..)
) where

import Control.Monad.IO.Class.Lift
import Data.Coerce
import Data.Foldable (toList)
import Data.Proxy
import qualified Foreign.Marshal.Array.Lift as A
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable as S
import GL.Error
import GL.Object
import GL.Range
import GL.Scalar
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Buffer (ty :: Type) = Buffer { unBuffer :: GLuint }
  deriving (Storable)

instance Object (Buffer ty) where
  gen n = glGenBuffers n . coerce
  delete n = glDeleteBuffers n . coerce

instance KnownType ty => Bind (Buffer ty) where
  nullObject = Buffer 0
  bindObject = checkingGLError . runLiftIO . glBindBuffer (typeToGLEnum (typeVal (Proxy :: Proxy ty))) . unBuffer

realloc :: forall ty n v m buffer sig . (KnownType ty, Scalar n, Foldable v, Has (Lift IO) sig m) => buffer ty -> [v n] -> Update -> Usage -> m ()
realloc _ vertices update usage = A.withArrayLen (vertices >>= toList) $ \ n p ->
  runLiftIO (glBufferData (typeToGLEnum (typeVal (Proxy @ty))) (fromIntegral (n * S.sizeOf @n 0)) (castPtr p) (hintToGLEnum update usage))

copy :: forall ty a m buffer sig . (KnownType ty, Has (Lift IO) sig m) => buffer ty -> Range -> Ptr a -> m ()
copy _ (Range offset size) = checkingGLError . runLiftIO . glBufferSubData (typeToGLEnum (typeVal (Proxy @ty))) (fromIntegral offset) (fromIntegral size) . castPtr


data Type
  = Array
  deriving (Eq, Ord, Show)

class KnownType (ty :: Type) where
  typeVal :: proxy ty -> Type

instance KnownType 'Array where
  typeVal _ = Array

typeToGLEnum :: Type -> GLenum
typeToGLEnum Array = GL_ARRAY_BUFFER


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
