{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Stochastic.Histogram
( histogram
, histogram2
, sparkify
, sparkify2
, printHistogram
, printHistogram2
) where

import           Control.Effect.Lift
import           Control.Lens (ix, (&), (+~), (^.))
import           Control.Monad (replicateM)
import           Data.Foldable (foldl')
import           Data.Functor.I
import           Data.Functor.Interval
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import           Linear.V2
import           System.Console.Terminal.Size as Size

histogram :: RealFrac a => Interval I a -> I Int -> [I a] -> U.Vector Int
histogram interval n = foldl' bucket (U.replicate (getI n) 0)
  where
  which sample = floor (toUnit interval sample * fromIntegral n)
  bucket accum sample = accum & ix (which sample) +~ 1

histogram2 :: RealFrac a => Interval V2 a -> V2 Int -> [V2 a] -> V.Vector (U.Vector Int)
histogram2 interval n = foldl' bucket (V.replicate (n ^. _y) (U.replicate (n ^. _x) 0))
  where
  which sample = fmap floor . (*) <$> toUnit interval sample <*> (fromIntegral <$> n)
  bucket accum sample = accum & ix x.ix y +~ 1 where
    V2 x y = which sample

sparkify :: [Int] -> String
sparkify bins = sparkifyRelativeTo (maximum bins) bins

sparkify2 :: [[Int]] -> [String]
sparkify2 bins = map (sparkifyRelativeTo max) bins
  where
  max = maximum (map maximum bins)

sparkifyRelativeTo :: Int -> [Int] -> String
sparkifyRelativeTo max bins
  | null bins = ""
  | otherwise = spark <$> bins
  where
  sparks = " ▁▂▃▄▅▆▇█"
  maxSpark = fromIntegral $ length sparks - 1
  spark n = sparks !! round ((fromIntegral n / fromIntegral max :: Double) * maxSpark)

printHistogram :: (RealFrac a, Has (Lift IO) sig m) => Interval I a -> Int -> m a -> m ()
printHistogram interval n m = do
  s <- maybe 80 Size.width <$> sendM Size.size
  samples <- replicateM n (fmap I m)
  sendM (putStrLn (sparkify (U.toList (histogram interval s samples))))

printHistogram2 :: (RealFrac a, Has (Lift IO) sig m) => Interval V2 a -> Int -> m (V2 a) -> m ()
printHistogram2 interval n m = do
  s <- maybe 80 (pure . Size.width) <$> sendM Size.size
  samples <- replicateM n m
  sendM (putStrLn (unlines (sparkify2 (map U.toList (V.toList (histogram2 interval s samples))))))
