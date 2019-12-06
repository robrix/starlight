{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module GL.Object
( Object(..)
, withN
, with
) where

import qualified Control.Exception as E
import Data.Coerce (coerce)
import qualified Foreign.Marshal.Array as A
import Foreign.Ptr
import Graphics.GL.Types

class Object t where
  construct :: GLuint -> t
  gen :: GLsizei -> Ptr t -> IO ()
  delete :: GLsizei -> Ptr t -> IO ()

withN :: forall t a . Object t => Int -> ([t] -> IO a) -> IO a
withN n = withObjects (coerce (gen @t)) (coerce (delete @t)) n . (. fmap (construct @t))

with :: Object t => (t -> IO a) -> IO a
with = withN 1 . (. head)

withObjects :: (GLsizei -> Ptr GLuint -> IO ()) -> (GLsizei -> Ptr GLuint -> IO ()) -> Int -> ([GLuint] -> IO a) -> IO a
withObjects gen delete n = E.bracket acquire release
  where acquire = A.allocaArray n $ \ p -> do
          gen (fromIntegral n) p
          A.peekArray n p
        release buffers = A.allocaArray n $ \ p -> do
          A.pokeArray p buffers
          delete (fromIntegral n) p
