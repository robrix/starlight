{-# LANGUAGE AllowAmbiguousTypes #-}
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
import           Control.Carrier.State.Strict
import           Control.Monad.IO.Class.Lift
import           Control.Monad.Trans.Class
import           Data.Functor.I
import           Data.Functor.Interval
import qualified Foreign.Marshal.Array.Lift as A
import           Foreign.Ptr (castPtr, nullPtr)
import           Foreign.Storable as S
import           GL.Effect.Check
import           GL.Enum as GL
import           GL.Object
import           Graphics.GL.Core41
import           Graphics.GL.Types
import           Linear.Vector

newtype Buffer (ty :: Type) v = Buffer { unBuffer :: GLuint }

instance Object (Buffer ty v) where
  gen = defaultGenWith glGenBuffers Buffer
  delete = defaultDeleteWith glDeleteBuffers unBuffer

instance KnownType ty => Bind (Buffer ty v) where
  bind = checking . runLiftIO . glBindBuffer (glEnum (typeVal @ty)) . maybe 0 unBuffer

-- FIXME: Store the current size and don’t reallocate when larger.
realloc :: forall ty v m sig . (HasBuffer ty v m, KnownType ty, S.Storable v, Has (Lift IO) sig m) => Int -> Update -> Usage -> m ()
realloc n update usage = askBuffer @ty >> runLiftIO (glBufferData (glEnum (typeVal @ty)) (fromIntegral (n * S.sizeOf @v undefined)) nullPtr (glEnum (Hint update usage)))

copy :: forall ty v m sig . (HasBuffer ty v m, KnownType ty, S.Storable v, Has Check sig m, Has (Lift IO) sig m) => Int -> [v] -> m ()
copy offset vertices = askBuffer @ty >> A.withArray vertices
  (checking . runLiftIO . glBufferSubData (glEnum (typeVal @ty)) (fromIntegral (min' i)) (fromIntegral (size i)) . castPtr) where
  i = (Interval 0 (I (length vertices)) + pure offset) ^* S.sizeOf @v undefined


data Type
  = Array
  | ElementArray
  deriving (Eq, Ord, Show)

class KnownType (ty :: Type) where
  typeVal :: Type

instance KnownType 'Array where
  typeVal = Array

instance KnownType 'ElementArray where
  typeVal = ElementArray

instance GL.Enum Type where
  glEnum = \case
    Array        -> GL_ARRAY_BUFFER
    ElementArray -> GL_ELEMENT_ARRAY_BUFFER


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


bindBuffer :: (KnownType ty, Has Check sig m, Has (Lift IO) sig m) => Buffer ty v -> BufferC ty v m a -> m a
bindBuffer buffer (BufferC m) = do
  bind (Just buffer)
  a <- runReader buffer m
  a <$ bind (Nothing `asTypeOf` Just buffer)

class Monad m => HasBuffer ty v m | m ty -> v where
  askBuffer :: m (Buffer ty v)

instance HasBuffer ty i m => HasBuffer ty i (ReaderC r m) where
  askBuffer = lift askBuffer

instance HasBuffer ty i m => HasBuffer ty i (StateC s m) where
  askBuffer = lift askBuffer


newtype BufferC ty v m a = BufferC { runBuffer :: ReaderC (Buffer ty v) m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadTrans)

instance Algebra sig m => HasBuffer ty v (BufferC ty v m) where
  askBuffer = BufferC ask

instance Algebra sig m => Algebra sig (BufferC ty v m) where
  alg = BufferC . send . handleCoercible
