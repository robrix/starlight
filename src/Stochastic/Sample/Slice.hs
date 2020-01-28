{-# LANGUAGE TypeApplications #-}
module Stochastic.Sample.Slice
( sample
) where

import           Control.Carrier.Random.Gen
import           Control.Carrier.State.Strict
import           Control.Lens ((&), (+~), (-~), (.~))
import           Data.Function (fix)
import           Data.Functor.I
import           Data.Functor.Interval
import           Stochastic.PDF
import qualified System.Random as R

sample :: (R.Random a, RealFrac a, Has Random sig m, Has (State (I a)) sig m) => I a -> I Int -> PDF (I a) (I a) -> m (I a)
sample w m (PDF pdf) = do
  x <- get
  y <- uniformR (0, pdf x)
  lr <- (\ u -> let l = x - w * u in Interval l (l + w)) <$> uniform
  jk <- (\ v -> let j = floor v in Interval j (m - 1 - j)) <$> uniformR @Double (0, fromIntegral m)
  let step i b
        | min' b > 0, y < pdf (min' i) = step (i & min_ -~ w) (b & min_ -~ 1)
        | max' b > 0, y < pdf (max' i) = step (i & max_ +~ w) (b & max_ -~ 1)
        | otherwise                    = i

  fix (\ shrink i -> do
    x' <- uniformI i
    if pdf x' > y then
      x' <$ put x'
    else if x' < x then
      shrink $ i & min_ .~ x'
    else
      shrink $ i & max_ .~ x')
    (step lr jk)
