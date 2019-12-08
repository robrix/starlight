{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeOperators, UndecidableInstances #-}
module GL.Carrier.Alloc
( -- * Alloc carrier
  runAlloc
, AllocC(..)
  -- * Alloc effect
, module GL.Effect.Alloc
) where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.State.IORef
import qualified Control.Exception.Lift as E
import Control.Monad.IO.Class
import Data.Foldable (sequenceA_)
import Data.IORef
import qualified Foreign.Marshal.Array.Lift as A
import GL.Effect.Alloc
import qualified GL.Object as GL

runAlloc :: Has (Lift IO) sig m => AllocC m a -> m a
runAlloc (AllocC m) = do
  ref <- sendM (newIORef [])
  runStateRef ref m `E.finally` (sendM (readIORef ref) >>= sequenceA_)

newtype AllocC m a = AllocC (StateC [m ()] m a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance Has (Lift IO) sig m => Algebra (Alloc :+: sig) (AllocC m) where
  alg = \case
    L (Gen n k) -> do
      bs <- acquire
      AllocC $ modify @[m ()] (release bs :)
      k bs where
      acquire = A.allocaArray n $ \ p -> do
        GL.gen (fromIntegral n) p
        A.peekArray n p
      release buffers = A.withArray buffers $ GL.delete (fromIntegral n)
    R other     -> AllocC (send (handleCoercible other))
