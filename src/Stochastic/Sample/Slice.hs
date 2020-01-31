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

sample
  :: ( Applicative f
     , Traversable f
     , Applicative g
     , Traversable g
     , R.Random a
     , R.Random b
     , Num a
     , Ord a
     , Num (f a)
     , Num (g b)
     , Ord (g b)
     , Has Random sig m
     , Has (State (f a)) sig m
     )
  => Interval f a
  -> Interval f a
  -> PDF (f a) (g b)
  -> m (f a)
sample w bounds (PDF pdf) = runReader w $ do
  x <- get
  y <- uniformI (Interval 0 (pdf x))
  u <- uniformI =<< ask
  size' <- asks size
  let step i
        | or ((>) <$> min' i <*> min' bounds), y < pdf (min' i) = step (i & min_ -~ size')
        | or ((<) <$> max' i <*> max' bounds), y < pdf (max' i) = step (i & max_ +~ size')
        | otherwise                                             = i
  local (intersection bounds . step . (point (x - u) +)) (shrink x y)
  where
  shrink x y = fix (\ go -> ask >>= uniformI >>= \case
    x' | y < pdf x' -> x' <$ put x'
       | otherwise  -> local (interval mn mx <*> point x <*> point x' <*>) go)
  mn x x' i = if x' < x then x' else i
  mx x x' i = if x' < x then i  else x'
