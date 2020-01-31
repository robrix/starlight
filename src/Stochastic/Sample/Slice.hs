{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Stochastic.Sample.Slice
( sample
) where

import           Control.Carrier.Random.Gen
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Lens ((&), (+~), (-~))
import           Data.Function (fix)
import           Data.Functor.Interval
import           Stochastic.PDF
import qualified System.Random as R

sample :: (Applicative f, Traversable f, Applicative g, Traversable g, R.Random a, R.Random b, Num (f a), Ord a, Num (g b), Ord (g b), Has Random sig m, Has (State (f a)) sig m) => Interval f a -> Interval f a -> PDF (f a) (g b) -> m (f a)
sample w m (PDF pdf) = runReader w $ do
  x <- get
  y <- uniformI (Interval 0 (pdf x))
  i <- ask >>= uniformI >>= step y . (x -)

  local (const (intersection i m)) (shrink x y)
  where
  step y l = do
    size' <- asks size
    fix (\ go i -> if
      | or ((>) <$> min' i <*> min' m), y < pdf (min' i) -> go (i & min_ -~ size')
      | or ((<) <$> max' i <*> max' m), y < pdf (max' i) -> go (i & max_ +~ size')
      | otherwise                                        -> pure i)
      (Interval l (l + size'))
  shrink x y = go
    where
    go = ask >>= uniformI >>= \case
      x' | y < pdf x' -> x' <$ put x'
         | otherwise  -> local (const (interval max min <*> point x <*> point x')) go
