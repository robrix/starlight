{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
module Stochastic.Sampler.Metropolis
( sample
, normal
, standard
) where

import           Control.Effect.Random
import           Data.Bits
import           Data.Function (fix)
import qualified Data.Vector.Unboxed as I
import           Data.Word
import           Stochastic.PDF
import qualified System.Random as R

sample :: (R.Random b, Fractional b, Ord b, Has Random sig m) => a -> (a -> m a) -> PDF a b -> m a
sample x0 proposal (PDF pdf) = fix (\ loop x -> do
  x' <- proposal x
  let alpha = pdf x' / pdf x
  u <- uniform
  if u <= alpha then
    pure x'
  else
    loop x) x0


-- | Generate a normally distributed random variate with given mean
-- and standard deviation.
normal :: Has Random sig m
       => Double                -- ^ Mean
       -> Double                -- ^ Standard deviation
       -> m Double
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
standard :: Has Random sig m => m Double
standard = loop where
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
        if e + c * (d - e) < 1
          then pure x
          else loop
  normalTail neg = tailing
    where
    tailing = do
      x <- (/rNorm) . log <$> uniform
      y <- log            <$> uniform
      if y * (-2) < x * x then
        tailing
      else
        pure $! if neg then x - rNorm else rNorm - x
{-# INLINE standard #-}


-- Constants used by standard/normal. They are floated to the top
-- level to avoid performance regression (Bug #16) when blocks/ratios
-- are recalculated on each call to standard/normal. It's also
-- somewhat difficult to trigger reliably.
blocks :: I.Vector Double
blocks = (`I.snoc` 0) . I.cons (v/f) . I.cons rNorm . I.unfoldrN 126 go $! T rNorm f
  where
    go (T b g) = let !u = T h (exp (-0.5 * h * h))
                     h  = sqrt (-2 * log (v / b + g))
                 in Just (h, u)
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
