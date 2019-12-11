module Control.Carrier.Time
( -- * Time carrier
  TimeC(..)
  -- * Time effect
, module Control.Effect.Time
) where

import Control.Effect.Time

newtype TimeC m a = TimeC (m a)
