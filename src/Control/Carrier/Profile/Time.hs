{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.Profile.Time
( -- * Profile carrier
  ProfileC(..)
, Timing(..)
, Timings(..)
  -- * Profile effect
, module Control.Effect.Profile
) where

import           Control.Effect.Profile
import           Control.Monad.IO.Class
import qualified Data.Map as Map
import           Data.Time.Clock

newtype ProfileC m a = ProfileC (m a)
  deriving (Applicative, Functor, Monad, MonadIO)


data Timing = Timing
  { timing :: !NominalDiffTime
  , count  :: {-# UNPACK #-} !Int
  }

instance Semigroup Timing where
  Timing t1 c1 <> Timing t2 c2 = Timing (t1 + t2) (c1 + c2)

instance Monoid Timing where
  mempty = Timing 0 0


newtype Timings = Timings (Map.Map String Timing)
