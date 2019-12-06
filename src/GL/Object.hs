{-# LANGUAGE FlexibleContexts #-}
module GL.Object
( Object(..)
, withN
, with
) where

import Control.Carrier.Lift
import qualified Control.Exception.Lift as E
import Control.Monad.IO.Class
import qualified Foreign.Marshal.Array.Lift as A
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL.Types

class Storable t => Object t where
  gen :: MonadIO m => GLsizei -> Ptr t -> m ()
  delete :: MonadIO m => GLsizei -> Ptr t -> m ()

withN :: (MonadIO m, Has (Lift IO) sig m, Object t) => Int -> ([t] -> m a) -> m a
withN n = E.bracket acquire release where
  acquire = A.allocaArray n $ \ p -> do
    gen (fromIntegral n) p
    A.peekArray n p
  release buffers = A.allocaArray n $ \ p -> do
    A.pokeArray p buffers
    delete (fromIntegral n) p

with :: (MonadIO m, Has (Lift IO) sig m, Object t) => (t -> m a) -> m a
with = withN 1 . (. head)
