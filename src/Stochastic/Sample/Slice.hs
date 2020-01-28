{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Stochastic.Sample.Slice
( sample
) where

import           Control.Carrier.Random.Gen
import           Control.Carrier.State.Strict
import           Control.Lens ((&), (+~), (-~), (.~))
import           Data.Functor.I
import           Data.Functor.Interval
import           Stochastic.PDF
import qualified System.Random as R

sample :: (R.Random a, RealFrac a, Has Random sig m, Has (State (I a)) sig m) => I a -> I Int -> PDF (I a) (I a) -> m (I a)
sample w m (PDF pdf) = do
  x <- get
  y <- uniformR (0, pdf x)
  i <- step x y <$> uniform <*> uniformR @Double (0, fromIntegral m)

  shrink x y i
  where
  step x y u v = go (Interval l (l + w)) (Interval j (m - 1 - j))
    where
    go i b
      | min' b > 0, y < pdf (min' i) = go (i & min_ -~ w) (b & min_ -~ 1)
      | max' b > 0, y < pdf (max' i) = go (i & max_ +~ w) (b & max_ -~ 1)
      | otherwise                    = i
    l = x - w * u
    j = floor v
  shrink x y = go
    where
    go i = uniformI i >>= \case
      x' | pdf x' > y -> x' <$ put x'
         | x' < x     -> go (i & min_ .~ x')
         | otherwise  -> go (i & max_ .~ x')
