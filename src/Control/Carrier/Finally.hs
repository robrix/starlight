{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.Finally
( -- * Finally carrier
  runFinally
, FinallyC(..)
  -- * Finally effect
, module Control.Effect.Finally
) where

import Control.Carrier.State.IORef
import Control.Effect.Finally
import qualified Control.Exception.Lift as E
import Control.Monad.IO.Class.Lift
import Data.Foldable (sequenceA_)
import Data.IORef

runFinally :: Has (Lift IO) sig m => FinallyC m a -> m a
runFinally (FinallyC m) = do
  ref <- sendM (newIORef [])
  runStateRef ref m `E.finally` (sendM (readIORef ref) >>= sequenceA_)

newtype FinallyC m a = FinallyC (StateC [m ()] m a)
  deriving (Applicative, Functor, Monad, MonadIO)
