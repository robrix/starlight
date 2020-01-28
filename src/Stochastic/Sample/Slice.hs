{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Stochastic.Sample.Slice
( sample
) where

import           Control.Carrier.Random.Gen
import           Control.Carrier.State.Strict
import           Control.Lens ((&), (+~), (-~), (.~))
import           Data.Functor.Interval
import           Stochastic.PDF
import qualified System.Random as R

sample :: (Applicative f, Traversable f, R.Random a, RealFrac (f a), Has Random sig m, Has (State (f a)) sig m) => Interval f a -> Interval f a -> PDF (f a) (f a) -> m (f a)
sample w m (PDF pdf) = do
  x <- get
  y <- uniformI (Interval 0 (pdf x))
  i <- step x y <$> uniformI w

  shrink x y i
  where
  step x y u = go (Interval l (l + size w))
    where
    go i
      | min' i > min' m, y < pdf (min' i) = go (i & min_ -~ size w)
      | max' i < max' m, y < pdf (max' i) = go (i & max_ +~ size w)
      | otherwise                         = i
    l = x - u
  shrink x y = go
    where
    go i = uniformI i >>= \case
      x' | pdf x' > y -> x' <$ put x'
         | x' < x     -> go (i & min_ .~ x')
         | otherwise  -> go (i & max_ .~ x')
