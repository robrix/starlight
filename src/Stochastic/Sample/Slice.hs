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
  i <- step x y <$> uniform <*> uniformR @Double (0, fromIntegral m)

  fix (\ shrink i -> do
    x' <- uniformI i
    if pdf x' > y then
      x' <$ put x'
    else if x' < x then
      shrink $ i & min_ .~ x'
    else
      shrink $ i & max_ .~ x')
    i
  where
  step x y u v = go (Interval l (l + w)) (Interval j (m - 1 - j))
    where
      go i b
        | min' b > 0, y < pdf (min' i) = go (i & min_ -~ w) (b & min_ -~ 1)
        | max' b > 0, y < pdf (max' i) = go (i & max_ +~ w) (b & max_ -~ 1)
        | otherwise                    = i
      l = x - w * u
      j = floor v
