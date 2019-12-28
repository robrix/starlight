{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.Profile.Time
( -- * Profile carrier
  ProfileC(..)
, Tally(..)
  -- * Profile effect
, module Control.Effect.Profile
) where

import Control.Effect.Profile
import Control.Monad.IO.Class

newtype ProfileC m a = ProfileC (m a)
  deriving (Applicative, Functor, Monad, MonadIO)


data Tally a = Tally
  { tally :: !a
  , count :: {-# UNPACK #-} !Int
  }

instance Num a => Semigroup (Tally a) where
  Tally t1 c1 <> Tally t2 c2 = Tally (t1 + t2) (c1 + c2)
