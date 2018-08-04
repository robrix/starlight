module GL.Object where

import qualified Control.Exception as E
import qualified Foreign.Marshal.Array as A
import Foreign.Ptr
import GHC.Stack
import Graphics.GL.Types

class Object t where
  characterize :: (GLuint -> t, GLsizei -> Ptr GLuint -> IO (), GLsizei -> Ptr GLuint -> IO ())

  withN :: HasCallStack => Int -> ([t] -> IO a) -> IO a
  withN n = let (construct, gen, delete) = characterize in withObjects gen delete n . (. fmap construct)

  with :: HasCallStack => (t -> IO a) -> IO a
  with = withN 1 . (. head)

withObjects :: (GLsizei -> Ptr GLuint -> IO ()) -> (GLsizei -> Ptr GLuint -> IO ()) -> Int -> ([GLuint] -> IO a) -> IO a
withObjects gen delete n = E.bracket acquire release
  where acquire = A.allocaArray n $ \ p -> do
          gen (fromIntegral n) p
          A.peekArray n p
        release buffers = A.allocaArray n $ \ p -> do
          A.pokeArray p buffers
          delete (fromIntegral n) p
