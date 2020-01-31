{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Stochastic.Sample.Slice
( sample
) where

import           Control.Applicative (liftA2)
import           Control.Carrier.Random.Gen
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Lens (over, (&))
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
     , Num b
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
  y <- uniformI (Interval (pure 0) (pdf x))
  u <- uniformI =<< ask
  size' <- asks size
  let step i
        -- if any coordinate of the interval’s min is in-bounds & it still lies under the curve, step the min outwards
        | or (min' i ^>^ min' bounds), y < pdf (min' i) = step (i & over min_ (^-^ size'))
        -- if any coordinate of the interval’s max is in-bounds & it still lies under the curve, step the max outwards
        | or (max' i ^<^ max' bounds), y < pdf (max' i) = step (i & over max_ (^+^ size'))
        | otherwise                                     = i
      shrink = ask >>= uniformI >>= \case
        x' | y < pdf x' -> x' <$ put x'
           | otherwise  -> local (interval mn mx <*> point x <*> point x' <*>) shrink
      mn x x' i = if x' < x then x' else i
      mx x x' i = if x' < x then i  else x'
      (^-^) = liftA2 (-)
      (^+^) = liftA2 (+)
      (^<^) = liftA2 (<)
      (^>^) = liftA2 (>)

  local (intersection bounds . step . (point (x ^-^ u) +)) shrink
