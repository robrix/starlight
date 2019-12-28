module Control.Carrier.Profile.Time
( -- * Profile carrier
  ProfileC(..)
  -- * Profile effect
, module Control.Effect.Profile
) where

import Control.Effect.Profile

newtype ProfileC m a = ProfileC (m a)
