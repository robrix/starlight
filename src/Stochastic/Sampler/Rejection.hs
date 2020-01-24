module Stochastic.Sampler.Rejection
( sample
) where

import           Control.Effect.Random
import           Data.Function (fix)
import           Stochastic.PDF
import qualified System.Random as R

sample :: (R.Random b, Num b, Ord b, Has Random sig m) => m a -> b -> PDF a b -> m a
sample sample maxPdf pdf = fix $ \ loop -> do
  x <- sample
  y <- uniformR (0, maxPdf)
  if y <= runPDF pdf x then
    pure x
  else
    loop
