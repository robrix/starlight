module Control.Effect.Time
( Time(..)
) where

import Data.Time.Clock

data Time m k
  = Now (UTCTime -> m k)
