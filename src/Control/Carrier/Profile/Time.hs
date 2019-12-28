{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Carrier.Profile.Time
( -- * Profile carrier
  ProfileC(..)
, Tally(..)
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


data Tally a = Tally
  { tally :: !a
  , count :: {-# UNPACK #-} !Int
  }

instance Num a => Semigroup (Tally a) where
  Tally t1 c1 <> Tally t2 c2 = Tally (t1 + t2) (c1 + c2)

instance Num a => Monoid (Tally a) where
  mempty = Tally 0 0


newtype Timings = Timings (Map.Map String (Tally NominalDiffTime))
