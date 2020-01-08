{-# LANGUAGE FlexibleContexts #-}
module GL.Object
( Object(..)
, Bind(..)
, genN
, gen1
, defaultGenWith
, defaultDeleteWith
) where

import           Control.Carrier.Lift
import           Control.Effect.Finally
import qualified Foreign.Marshal.Array.Lift as A
import           Foreign.Ptr
import           GHC.Stack
import           GL.Effect.Check
import           Graphics.GL.Types

class Object t where
  gen :: (Has (Lift IO) sig m, HasCallStack) => Int -> Ptr GLuint -> m [t]
  delete :: (Has (Lift IO) sig m, HasCallStack) => [t] -> Ptr GLuint -> m ()

class Bind t where
  bind :: (Has Check sig m, Has (Lift IO) sig m, HasCallStack) => Maybe t -> m ()

genN :: (Object t, Has Finally sig m, Has (Lift IO) sig m) => Int -> m [t]
genN n = do
  ts <- acquire
  ts <$ onExit (release ts) where
  acquire = A.allocaArray n $ gen n
  release ts = A.allocaArray n $ delete ts

gen1 :: (Object t, Has Finally sig m, Has (Lift IO) sig m) => m t
gen1 = head <$> genN 1


defaultGenWith :: Has (Lift IO) sig m => (GLsizei -> Ptr GLuint -> IO ()) -> (GLuint -> t) -> Int -> Ptr GLuint -> m [t]
defaultGenWith with make n ptr = sendM (with (fromIntegral n) ptr) >> map make <$> A.peekArray n ptr

defaultDeleteWith :: Has (Lift IO) sig m => (GLsizei -> Ptr GLuint -> IO ()) -> (t -> GLuint) -> [t] -> Ptr GLuint -> m ()
defaultDeleteWith with get bs ptr = A.pokeArray ptr (map get bs) >> sendM (with (fromIntegral (length bs)) ptr)
