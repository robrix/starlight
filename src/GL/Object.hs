module GL.Object
( Object(..)
, withN
, with
) where

import qualified Control.Exception as E
import Control.Monad.IO.Class
import qualified Foreign.Marshal.Array as A
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL.Types

class Storable t => Object t where
  gen :: MonadIO m => GLsizei -> Ptr t -> m ()
  delete :: MonadIO m => GLsizei -> Ptr t -> m ()

withN :: Object t => Int -> ([t] -> IO a) -> IO a
withN n = E.bracket acquire release where
  acquire = A.allocaArray n $ \ p -> do
    gen (fromIntegral n) p
    A.peekArray n p
  release buffers = A.allocaArray n $ \ p -> do
    A.pokeArray p buffers
    delete (fromIntegral n) p

with :: Object t => (t -> IO a) -> IO a
with = withN 1 . (. head)
