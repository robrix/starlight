module Starlight.Time
( now
, since
, timed
) where

import Control.Carrier.Reader
import Control.Effect.Lift
import Control.Effect.State
import Data.Time.Clock
import Unit.Time

now :: Has (Lift IO) sig m => m UTCTime
now = sendM getCurrentTime

since :: Has (Lift IO) sig m => UTCTime -> m NominalDiffTime
since t = flip diffUTCTime t <$> now

timed
  :: ( Has (Lift IO) sig m
     , Has (State UTCTime) sig m
     )
  => ReaderC (Seconds Double) m a
  -> m a
timed m = do
  dt <- fmap realToFrac . since =<< get
  put =<< now
  runReader dt m
