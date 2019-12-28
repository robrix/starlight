{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.Profile.Identity
( -- * Profiling carrier
  runProfile
, ProfileC(ProfileC)
) where

import Control.Monad.IO.Class

newtype ProfileC m a = ProfileC { runProfile :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)
