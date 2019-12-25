{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
) where

import           Control.Monad.IO.Class.Lift
import           Data.Coerce
import           Data.Interval
import           Data.Proxy
import qualified Foreign.Marshal.Array.Lift as A
import           Foreign.Ptr (Ptr, castPtr, nullPtr)
import           Foreign.Storable as S
import           GL.Enum as GL
import           GL.Error
import           GL.Object
import           Graphics.GL.Core41
import           Graphics.GL.Types
import           Linear.Vector

newtype Buffer (ty :: Type) v = Buffer { unBuffer :: GLuint }
  deriving (Storable)

instance Object (Buffer ty v) where
  gen n = runLiftIO . glGenBuffers n . coerce
  delete n = runLiftIO . glDeleteBuffers n . coerce

instance KnownType ty => Bind (Buffer ty v) where
  bind = checkingGLError . runLiftIO . glBindBuffer (glEnum (typeVal (Proxy :: Proxy ty))) . maybe 0 unBuffer

realloc :: (KnownType ty, S.Storable v, Has (Lift IO) sig m) => Buffer ty v -> Int -> Update -> Usage -> m ()
realloc b n update usage = runLiftIO (glBufferData (glEnum (typeOf b)) (fromIntegral (n * sizeOfElem b)) nullPtr (hintToGLEnum update usage))

copyPtr :: (KnownType ty, Has (Lift IO) sig m) => Buffer ty a -> Interval Int -> Ptr a -> m ()
copyPtr b i = checkingGLError . runLiftIO . glBufferSubData (glEnum (typeOf b)) (fromIntegral (min_ i)) (fromIntegral (size i)) . castPtr

copy :: (KnownType ty, S.Storable v, Has (Lift IO) sig m) => Buffer ty v -> Int -> [v] -> m ()
copy b offset vertices = A.withArray vertices $ copyPtr b ((Interval 0 (length vertices) + pure offset) ^* sizeOfElem b)

typeOf :: KnownType ty => Buffer ty a -> Type
typeOf b = typeVal (typeProxy b)
  where typeProxy :: Buffer ty a -> Proxy ty
        typeProxy _ = Proxy

sizeOfElem :: Storable a => Buffer ty a -> Int
sizeOfElem b = S.sizeOf (elem b)
  where elem :: Buffer ty a -> a
        elem _ = undefined


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
