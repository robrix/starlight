module Control.Concurrent.Lift
( runInBoundThread
) where

import qualified Control.Concurrent as CC
import           Control.Effect.Lift

-- | See @"Control.Concurrent".'CC.runInBoundThread'@.
runInBoundThread :: Has (Lift IO) sig m => m a -> m a
runInBoundThread m = liftWith $ \ hdl ctx -> CC.runInBoundThread (hdl (m <$ ctx))
