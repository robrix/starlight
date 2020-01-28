{-# LANGUAGE TypeApplications #-}
module Stochastic.Sample.Slice
( sample
) where

import           Control.Carrier.Random.Gen
import           Control.Carrier.State.Strict
import           Data.Function (fix)
import           Stochastic.PDF
import qualified System.Random as R

sample :: (R.Random a, RealFrac a, Has Random sig m, Has (State a) sig m) => a -> Int -> PDF a a -> m a
sample w m (PDF pdf) = do
  x <- get
  y <- uniformR (0, pdf x)
  u <- uniform
  v <- uniformR @Double (0, fromIntegral m)
  let l = x - w * u
      r = l + w
      j = floor @_ @Int v
      k = m - 1 - j
      step l r j k
        | j > 0, y < pdf l = step (l - w) r (j - 1) k
        | k > 0, y < pdf r = step l (r + w) j (k - 1)
        | otherwise        = (l, r)

  fix (\ shrink (l, r) -> do
    x' <- uniformR (l, r)
    if pdf x' > y then
      pure x'
    else if x' < x then
      shrink (x', r)
    else
      shrink (l, x'))
    (step l r j k)
