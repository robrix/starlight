{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module GL.Object
( Object(..)
, withN
, Bind(..)
, bind
) where

import qualified Control.Exception.Lift as E
import Control.Monad.IO.Class.Lift
import GHC.Stack
import qualified Foreign.Marshal.Array.Lift as A
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL.Types

class Storable t => Object t where
  gen :: MonadIO m => GLsizei -> Ptr t -> m ()
  delete :: MonadIO m => GLsizei -> Ptr t -> m ()

withN :: (Has (Lift IO) sig m, Object t) => Int -> ([t] -> m a) -> m a
withN n = E.bracket acquire release where
  acquire = A.allocaArray n $ \ p -> runLiftIO $ do
    gen (fromIntegral n) p
    A.peekArray n p
  release buffers = A.withArray buffers $ runLiftIO . delete (fromIntegral n)


class Bind t where
  nullObject :: t
  bindObject :: (Has (Lift IO) sig m, HasCallStack) => t -> m ()

bind :: forall t m a sig . (Bind t, Has (Lift IO) sig m, HasCallStack) => t -> m a -> m a
bind t m = do
  bindObject t
  a <- m
  a <$ bindObject (nullObject :: t)
