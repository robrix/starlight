{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Profile.Time
( -- * Profile carrier
  runProfile
, ProfileC(ProfileC)
, Timing(..)
, mean
, Timings(..)
  -- * Profile effect
, module Control.Effect.Profile
) where

import           Control.Algebra
import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Control.Carrier.Writer.Strict
import           Control.Effect.Profile
import           Control.Monad.IO.Class
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as Map
import           Data.Text (Text)
import           Data.Time.Clock
import           Prelude hiding (sum)

runProfile :: ProfileC m a -> m (Timings, a)
runProfile (ProfileC m) = runWriter (runReader [] m)

newtype ProfileC m a = ProfileC { runProfileC :: ReaderC [Text] (WriterC Timings m) a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance (Has (Lift IO) sig m, Effect sig) => Algebra (Profile :+: sig) (ProfileC m) where
  alg = \case
    L (Measure l m k) -> do
      start <- sendM getCurrentTime
      a <- ProfileC (local (l:) (runProfileC m))
      end <- sendM getCurrentTime
      ls <- ProfileC ask
      ProfileC (tell (timing (l:|ls) (end `diffUTCTime` start)))
      k a
    R other -> ProfileC (send (handleCoercible other))
    where
    timing ls t = Timings (Map.singleton ls (Timing t t t 1))


data Timing = Timing
  { sum   :: !NominalDiffTime
  , min'  :: !NominalDiffTime
  , max'  :: !NominalDiffTime
  , count :: {-# UNPACK #-} !Int
  }

instance Semigroup Timing where
  Timing s1 mn1 mx1 c1 <> Timing s2 mn2 mx2 c2 = Timing (s1 + s2) (mn1 `min` mn2) (mx1 `max` mx2) (c1 + c2)

instance Monoid Timing where
  mempty = Timing 0 0 0 0

mean :: Timing -> NominalDiffTime
mean Timing{ sum, count } = sum / fromIntegral count


newtype Timings = Timings (Map.Map (NonEmpty Text) Timing)

instance Semigroup Timings where
  Timings t1 <> Timings t2 = Timings (Map.unionWith (<>) t1 t2)

instance Monoid Timings where
  mempty = Timings mempty
