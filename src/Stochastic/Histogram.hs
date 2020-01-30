{-# LANGUAGE FlexibleContexts #-}
module Stochastic.Histogram
( histogram
, sparkify
, printHistogram
) where

import Control.Effect.Lift
import Control.Lens (ix, (&), (+~))
import Control.Monad (replicateM)
import Data.Foldable (foldl')
import Data.Functor.I
import Data.Functor.Interval
import System.Console.Terminal.Size as Size

histogram :: RealFrac a => Interval I a -> I Int -> [I a] -> [Int]
histogram interval n = foldl' bucket (replicate (getI n) 0)
  where
  which sample = floor (toUnit interval sample * fromIntegral n)
  bucket accum sample = accum & ix (which sample) +~ 1

sparkify :: [Int] -> String
sparkify bins
  | null bins = ""
  | otherwise = spark <$> bins
  where
  sparks = " ▁▂▃▄▅▆▇█"
  maxSpark = fromIntegral $ length sparks - 1
  max = fromIntegral $ maximum bins :: Double
  spark n = sparks !! round ((fromIntegral n / max) * maxSpark)

printHistogram :: (RealFrac a, Has (Lift IO) sig m) => Interval I a -> Int -> m a -> m ()
printHistogram interval n m = do
  s <- maybe 80 Size.width <$> sendM Size.size
  samples <- replicateM n (fmap I m)
  sendM (putStrLn (sparkify (histogram interval s samples)))
