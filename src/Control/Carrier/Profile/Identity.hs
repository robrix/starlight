module Control.Carrier.Profile.Identity
( -- * Profiling carrier
  runProfile
, ProfileC(ProfileC)
) where

newtype ProfileC m a = ProfileC { runProfile :: m a }
