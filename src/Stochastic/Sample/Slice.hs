{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Stochastic.Sample.Slice
( sample
) where

import           Control.Applicative (liftA2)
import           Control.Carrier.Random.Gen
import           Control.Carrier.State.Strict
import           Control.Lens (over, (&))
import           Data.Functor.Interval
import           Stochastic.PDF
import           Stochastic.Sample.Markov
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
     , Has (State (Chain (f a))) sig m
     )
  => Interval f a
  -> Interval f a
  -> PDF (f a) (g b)
  -> m (f a)
sample w bounds (PDF pdf) = do
  x <- gets getChain
  x <- if x `member` bounds then
    pure x
  else
    uniformI bounds
  y <- uniformI (Interval (pure 0) (pdf x))
  u <- uniformI w
  let step i
        -- if any coordinate of the interval’s min is in-bounds…
        | or (inf i ^>^ inf bounds)
        -- … & it still lies under the curve, step the min outwards
        , y < pdf (inf i) = step (i & over inf_ (^-^ size w))
        -- if any coordinate of the interval’s max is in-bounds…
        | or (sup i ^<^ sup bounds)
        -- … & it still lies under the curve, step the max outwards
        , y < pdf (sup i) = step (i & over sup_ (^+^ size w))
        | otherwise        = i
      shrink i = uniformI i >>= \case
        x' | y < pdf x' -> x' <$ put (Chain x')
           | otherwise  -> shrink ((mn...mx) <*> point x <*> point x' <*> i)
      mn x x' i = if x' < x then x' else i
      mx x x' i = if x' < x then i  else x'
      (^-^) = liftA2 (-)
      (^+^) = liftA2 (+)
      (^<^) = liftA2 (<)
      (^>^) = liftA2 (>)

  shrink (intersection bounds (step (point (x ^-^ u) + w)))
