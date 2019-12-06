{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module GL.Object
( Object(..)
, withN
, with
) where

import qualified Control.Exception as E
import qualified Foreign.Marshal.Array as A
import Foreign.Ptr
import Graphics.GL.Types

class Object t where
  construct :: GLuint -> t
  gen :: GLsizei -> Ptr t -> IO ()
  delete :: GLsizei -> Ptr t -> IO ()

withN :: forall t a . Object t => Int -> ([t] -> IO a) -> IO a
withN n = E.bracket acquire release . (. fmap (construct @t)) where
  acquire = A.allocaArray n $ \ p -> do
    gen @t (fromIntegral n) (castPtr p)
    A.peekArray n p
  release buffers = A.allocaArray n $ \ p -> do
    A.pokeArray p buffers
    delete @t (fromIntegral n) (castPtr p)

with :: Object t => (t -> IO a) -> IO a
with = withN 1 . (. head)
