{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
, HasBuffer
, askBuffer
) where

import           Control.Algebra.Dependent
import           Control.Carrier.Reader
import           Control.Monad.IO.Class.Lift
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
realloc :: forall ty v m sig . (HasBuffer ty v sig m, KnownType ty, S.Storable v, Has (Lift IO) sig m) => Int -> Update -> Usage -> m ()
realloc n update usage = askBuffer @ty >> runLiftIO (glBufferData (glEnum (typeVal @ty)) (fromIntegral (n * S.sizeOf @v undefined)) nullPtr (glEnum (Hint update usage)))

copy :: forall ty v m sig . (HasBuffer ty v sig m, KnownType ty, S.Storable v, Has Check sig m, Has (Lift IO) sig m) => Int -> [v] -> m ()
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
bindBuffer buffer m = do
  bind (Just buffer)
  a <- runReader buffer (runDep m)
  a <$ bind (Nothing `asTypeOf` Just buffer)

type HasBuffer ty v sig m = DHas (Buffer ty) (Reader (Buffer ty v)) sig m

askBuffer :: forall ty v m sig . DHas (Buffer ty) (Reader (Buffer ty v)) sig m => m (Buffer ty v)
askBuffer = runInDep @_ @(Buffer ty) ask

type BufferC ty v = Dep (Buffer ty) (ReaderC (Buffer ty v))
