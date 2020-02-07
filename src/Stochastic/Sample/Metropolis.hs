module Stochastic.Sample.Metropolis
( burnIn
, sample
) where

import           Control.Effect.Random
import           Control.Effect.State
import           Control.Monad (replicateM_)
import           Stochastic.PDF
import qualified System.Random as R

burnIn :: (R.Random b, Fractional b, Ord b, Has Random sig m, Has (State a) sig m) => Int -> (a -> m a) -> PDF a b -> m ()
burnIn i proposal = replicateM_ i . sample proposal


sample :: (R.Random b, Fractional b, Ord b, Has Random sig m, Has (State a) sig m) => (a -> m a) -> PDF a b -> m a
sample proposal (PDF pdf) = do
  x <- get
  x' <- proposal x
  let alpha = min 1 (pdf x' / pdf x)
  u <- uniform
  if u <= alpha then
    x' <$ put x'
  else
    pure x
