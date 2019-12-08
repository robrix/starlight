{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module GL.Object
( Object(..)
, Bind(..)
, bind
) where

import Control.Carrier.Lift
import GHC.Stack
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL.Types

class Storable t => Object t where
  gen :: (Has (Lift IO) sig m, HasCallStack) => GLsizei -> Ptr t -> m ()
  delete :: (Has (Lift IO) sig m, HasCallStack) => GLsizei -> Ptr t -> m ()

class Bind t where
  nullObject :: t
  bindObject :: (Has (Lift IO) sig m, HasCallStack) => t -> m ()

bind :: forall t m a sig . (Bind t, Has (Lift IO) sig m, HasCallStack) => t -> m a -> m a
bind t m = do
  bindObject t
  a <- m
  a <$ bindObject (nullObject :: t)
