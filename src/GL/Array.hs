{-# LANGUAGE ScopedTypeVariables #-}
module GL.Array where

import Data.Foldable (toList)
import Data.Proxy
import qualified Foreign.Marshal.Array as A
import Foreign.Ptr
import qualified Foreign.Storable as S
import GHC.Stack
import GL.Buffer
import GL.Object
import GL.Scalar
import Graphics.GL.Core41
import Graphics.GL.Types

newtype Array n = Array { unArray :: GLuint }

withArray :: forall v n a. (Foldable v, Scalar n, HasCallStack) => [v n] -> (Array n -> IO a) -> IO a
withArray vertices body = with $ \ buffer -> do
  glBindBuffer GL_ARRAY_BUFFER (unBuffer buffer)
  A.withArrayLen (vertices >>= toList) $ \ n p ->
    glBufferData GL_ARRAY_BUFFER (fromIntegral (n * S.sizeOf (0 :: n))) (castPtr p) GL_STATIC_DRAW
  with $ \ array -> do
    glBindVertexArray (unArray array)
    glEnableVertexAttribArray 0
    glVertexAttribPointer 0 (fromIntegral (length (head vertices))) (glType (Proxy :: Proxy n)) GL_FALSE 0 nullPtr
    body array

instance Object (Array n) where characterize = (Array, glGenVertexArrays, glDeleteVertexArrays)
