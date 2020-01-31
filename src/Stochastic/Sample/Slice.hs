{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Stochastic.Sample.Slice
( sample
) where

import           Control.Carrier.Random.Gen
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Lens ((+~), (-~))
import           Data.Function (fix)
import           Data.Functor.Interval
import           Stochastic.PDF
import qualified System.Random as R

sample :: (Applicative f, Traversable f, Applicative g, Traversable g, R.Random a, R.Random b, Num (f a), Ord a, Num (g b), Ord (g b), Has Random sig m, Has (State (f a)) sig m) => Interval f a -> Interval f a -> PDF (f a) (g b) -> m (f a)
sample w m (PDF pdf) = runReader w $ do
  x <- get
  y <- uniformI (Interval 0 (pdf x))
  u <- uniformI =<< ask
  step y (x - u) (local (intersection m) (shrink x y))
  where
  step y l m' = do
    size' <- asks size
    local (const (Interval l (l + size'))) $ fix (\ go -> ask >>= \ i -> if
      | or ((>) <$> min' i <*> min' m), y < pdf (min' i) -> local (min_ -~ size') go
      | or ((<) <$> max' i <*> max' m), y < pdf (max' i) -> local (max_ +~ size') go
      | otherwise                                        -> m')
  shrink x y = fix (\ go -> ask >>= uniformI >>= \case
    x' | y < pdf x' -> x' <$ put x'
       -- FIXME: we should be shrinking the interval pointwise based on pointwise x < x', rather than makng a new one
       | otherwise  -> local (shrinkI x x') go)
  shrinkI x x' i = Interval (mn <$> x <*> x' <*> min' i) (mx <$> x <*> x' <*> max' i)
    where
    mn x x' i = if x' < x then x' else i
    mx x x' i = if x' < x then i else x'
