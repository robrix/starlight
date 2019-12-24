{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GL.Object
( Object(..)
, Bind(..)
, genN
, gen1
) where

import           Control.Carrier.Lift
import           Control.Effect.Finally
import qualified Foreign.Marshal.Array.Lift as A
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Stack
import           Graphics.GL.Types

class Storable t => Object t where
  gen :: (Has (Lift IO) sig m, HasCallStack) => GLsizei -> Ptr t -> m ()
  delete :: (Has (Lift IO) sig m, HasCallStack) => GLsizei -> Ptr t -> m ()

class Bind t where
  bind :: (Has (Lift IO) sig m, HasCallStack) => Maybe t -> m ()

genN :: (Object t, Has Finally sig m, Has (Lift IO) sig m) => Int -> m [t]
genN n = do
  ts <- acquire
  ts <$ onExit (release ts) where
  acquire = A.allocaArray n $ \ p -> do
    gen (fromIntegral n) p
    A.peekArray n p
  release ts = A.withArray ts $ delete (fromIntegral n)

gen1 :: (Object t, Has Finally sig m, Has (Lift IO) sig m) => m t
gen1 = head <$> genN 1
