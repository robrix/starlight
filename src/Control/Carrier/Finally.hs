{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Control.Carrier.Finally
( -- * Finally carrier
  runFinally
, FinallyC(..)
  -- * Finally effect
, module Control.Effect.Finally
) where

import Control.Algebra
import Control.Carrier.State.IORef
import Control.Effect.Finally
import qualified Control.Exception.Lift as E
import Control.Monad.IO.Class.Lift
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.IORef

runFinally :: Has (Lift IO) sig m => FinallyC m a -> m a
runFinally (FinallyC m) = do
  ref <- sendM (newIORef [])
  runStateRef ref m `E.finally` (sendM (readIORef ref) >>= traverse_ runFinally)

newtype FinallyC m a = FinallyC (StateC [FinallyC m ()] m a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance Has (Lift IO) sig m => Algebra (Finally :+: sig) (FinallyC m) where
  alg = \case
    L (OnExit m k) -> FinallyC (modify (void m :)) >> k
    R other        -> FinallyC (send (handleCoercible other))
