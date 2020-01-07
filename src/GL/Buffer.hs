{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module GL.Buffer
( Buffer(..)
, realloc
, copy
, Type(..)
, KnownType(..)
, Update(..)
, Usage(..)
, bindBuffer
, HasBuffer(..)
, runBuffer
, BufferC(BufferC)
) where

import           Control.Algebra
import           Control.Carrier.Reader
import           Control.Monad.IO.Class.Lift
import           Control.Monad.Trans.Class
import           Data.Coerce
import           Data.Functor.Identity
import           Data.Functor.Interval
import           Data.Proxy
import qualified Foreign.Marshal.Array.Lift as A
import           Foreign.Ptr (Ptr, castPtr, nullPtr)
import           Foreign.Storable as S
import           GL.Effect.Check
import           GL.Enum as GL
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
  bind = checking . runLiftIO . glBindBuffer (glEnum (typeVal (Proxy :: Proxy ty))) . maybe 0 unBuffer

realloc :: (KnownType ty, S.Storable v, Has (Lift IO) sig m) => Buffer ty v -> Int -> Update -> Usage -> m ()
realloc b n update usage = runLiftIO (glBufferData (glEnum (typeOf b)) (fromIntegral (n * sizeOfElem b)) nullPtr (glEnum (Hint update usage)))

copyPtr :: (KnownType ty, Has Check sig m, Has (Lift IO) sig m) => Buffer ty a -> Interval Identity Int -> Ptr a -> m ()
copyPtr b i = checking . runLiftIO . glBufferSubData (glEnum (typeOf b)) (fromIntegral (min' i)) (fromIntegral (size i)) . castPtr

copy :: (KnownType ty, S.Storable v, Has Check sig m, Has (Lift IO) sig m) => Buffer ty v -> Int -> [v] -> m ()
copy b offset vertices = A.withArray vertices $ copyPtr b ((Interval 0 (Identity (length vertices)) + pure offset) ^* sizeOfElem b)

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

data Hint = Hint Update Usage

instance GL.Enum Hint where
  glEnum = \case
    Hint Static  Draw -> GL_STATIC_DRAW
    Hint Static  Read -> GL_STATIC_READ
    Hint Static  Copy -> GL_STATIC_COPY
    Hint Dynamic Draw -> GL_DYNAMIC_DRAW
    Hint Dynamic Read -> GL_DYNAMIC_READ
    Hint Dynamic Copy -> GL_DYNAMIC_COPY
    Hint Stream  Draw -> GL_STREAM_DRAW
    Hint Stream  Read -> GL_STREAM_READ
    Hint Stream  Copy -> GL_STREAM_COPY


bindBuffer :: (KnownType ty, Has Check sig m, Has (Lift IO) sig m) => Buffer ty (v Identity) -> BufferC ty v m a -> m a
bindBuffer buffer (BufferC m) = do
  bind (Just buffer)
  a <- runReader buffer m
  a <$ bind (Nothing `asTypeOf` Just buffer)

class Monad m => HasBuffer ty v m | m -> ty v where
  askBuffer :: m (Buffer ty (v Identity))

instance HasBuffer ty i m => HasBuffer ty i (ReaderC r m) where
  askBuffer = lift askBuffer


newtype BufferC ty v m a = BufferC { runBuffer :: ReaderC (Buffer ty (v Identity)) m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadTrans)

instance Algebra sig m => HasBuffer ty i (BufferC ty i m) where
  askBuffer = BufferC ask

instance Algebra sig m => Algebra sig (BufferC ty i m) where
  alg = BufferC . send . handleCoercible
