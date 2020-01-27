{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
module Stochastic.Sample.Metropolis
( burnIn
, sample
, normalPDF
, normal
, standard
, standardFrom
) where

import           Control.Effect.Random
import           Control.Effect.State
import           Control.Monad (replicateM_, when)
import           Data.Bits
import qualified Data.Vector.Unboxed as I
import           Data.Word
import           Stochastic.PDF
import qualified System.Random as R

burnIn :: (R.Random b, Fractional b, Ord b, Has Random sig m, Has (State a) sig m) => Int -> (a -> PDF a a) -> (a -> m a) -> PDF a b -> m ()
burnIn i proposalPDF proposal = replicateM_ i . sample proposalPDF proposal


sample :: (R.Random b, Fractional b, Ord b, Has Random sig m, Has (State a) sig m) => (a -> PDF a a) -> (a -> m a) -> PDF a b -> m a
sample proposalPDF proposal (PDF pdf) = do
  x <- get
  x' <- proposal x
  let alpha = pdf x' / pdf x
  u <- uniform
  x' <$ when (u <= alpha) (put x')


normalPDF :: Floating a => a -> a -> PDF a a
normalPDF mean stddev = PDF $ \ x ->
  (1 / (stddev * sqrt (2 * pi))) ** exp (-0.5 * ((x - mean) / stddev) ** 2)


-- Taken from mwc-random

-- | Generate a normally distributed random variate with given mean
-- and standard deviation.
normal :: (Fractional a, Has Random sig m)
       => a -- ^ Mean
       -> a -- ^ Standard deviation
       -> m a
normal m s = do
  x <- standard
  pure $! m + s * x
{-# INLINE normal #-}

-- | Generate a normally distributed random variate with zero mean and
-- unit variance.
--
-- The implementation uses Doornik's modified ziggurat algorithm.
-- Compared to the ziggurat algorithm usually used, this is slower,
-- but generates more independent variates that pass stringent tests
-- of randomness.
standard :: (Fractional a, Has Random sig m) => m a
standard = standardFrom uniform
{-# INLINE standard #-}

standardFrom :: (Fractional a, Monad m) => (forall a . R.Random a => m a) -> m a
standardFrom uniform = realToFrac <$> loop where
  loop = do
    u  <- subtract 1 . (*2) <$> uniform
    ri <- uniform
    let i  = fromIntegral ((ri :: Word32) .&. 127)
        bi = I.unsafeIndex blocks i
        bj = I.unsafeIndex blocks (i+1)
    if| abs u < I.unsafeIndex ratios i -> pure $! u * bi
      | i == 0                         -> normalTail (u < 0)
      | otherwise                      -> do
        let x  = u * bi
            xx = x * x
            d  = exp (-0.5 * (bi * bi - xx))
            e  = exp (-0.5 * (bj * bj - xx))
        c <- uniform
        if e + c * (d - e) < 1 then
          pure x
        else
          loop
  normalTail neg = tailing
    where
    tailing = do
      x <- (/rNorm) . log <$> uniform
      y <- log            <$> uniform
      if y * (-2) < x * x then
        tailing
      else
        pure $! if neg then x - rNorm else rNorm - x
{-# INLINE standardFrom #-}

-- Constants used by standard/normal. They are floated to the top
-- level to avoid performance regression (Bug #16) when blocks/ratios
-- are recalculated on each call to standard/normal. It's also
-- somewhat difficult to trigger reliably.
blocks :: I.Vector Double
blocks = (`I.snoc` 0) . I.cons (v/f) . I.cons rNorm . I.unfoldrN 126 go $! T rNorm f
  where
  go (T b g)
    | let !u = T h (exp (-0.5 * h * h))
          h  = sqrt (-2 * log (v / b + g))
    = Just (h, u)
  v = 9.91256303526217e-3
  f = exp (-0.5 * rNorm * rNorm)
{-# NOINLINE blocks #-}

rNorm :: Double
rNorm = 3.442619855899

ratios :: I.Vector Double
ratios = I.zipWith (/) (I.tail blocks) blocks
{-# NOINLINE ratios #-}

-- Unboxed 2-tuple
data T = T {-# UNPACK #-} !Double {-# UNPACK #-} !Double
