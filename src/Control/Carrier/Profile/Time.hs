{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.Profile.Time
( -- * Profile carrier
  runProfile
, ProfileC(..)
, Timing(..)
, Timings(..)
  -- * Profile effect
, module Control.Effect.Profile
) where

import           Control.Carrier.Writer.Strict
import           Control.Effect.Profile
import           Control.Monad.IO.Class
import qualified Data.Map as Map
import           Data.Time.Clock

runProfile :: ProfileC m a -> m (Timings, a)
runProfile (ProfileC m) = runWriter m

newtype ProfileC m a = ProfileC (WriterC Timings m a)
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

instance Semigroup Timings where
  Timings t1 <> Timings t2 = Timings (Map.unionWith (<>) t1 t2)

instance Monoid Timings where
  mempty = Timings mempty
