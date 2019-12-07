{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, KindSignatures, ScopedTypeVariables, TypeApplications #-}
module GL.Buffer
( Buffer(..)
, realloc
, Type(..)
, KnownType(..)
, typeToGLEnum
, Update(..)
, Usage(..)
) where

import Control.Monad.IO.Class.Lift
import Data.Coerce
import Data.Foldable (toList)
import Data.Proxy
import qualified Foreign.Marshal.Array.Lift as A
import Foreign.Ptr (castPtr)
import Foreign.Storable as S
import GL.Error
import GL.Object
import GL.Scalar
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

realloc :: forall ty n v m sig . (KnownType ty, Scalar n, Foldable v, Has (Lift IO) sig m) => Buffer ty n -> [v n] -> m ()
realloc buffer vertices = bind buffer $ A.withArrayLen (vertices >>= toList) $ \ n p ->
  runLiftIO (glBufferData (typeToGLEnum (typeVal (Proxy :: Proxy ty))) (fromIntegral (n * S.sizeOf @n 0)) (castPtr p) GL_STATIC_DRAW)


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
