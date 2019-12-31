{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Carrier.Profile.Time
( -- * Profile carrier
  runProfile
, ProfileC(ProfileC)
, Timing(..)
, renderTiming
, mean
, Timings(..)
, renderTimings
  -- * Profile effect
, module Control.Effect.Profile
) where

import           Control.Algebra
import           Control.Carrier.Lift
import           Control.Carrier.Writer.Strict
import           Control.Effect.Profile
import           Control.Monad.IO.Class
import           Data.List (sortOn)
import qualified Data.Map as Map
import           Data.Ord (Down(..))
import           Data.Text (Text)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Time.Clock
import           Numeric (showFFloat)
import           Prelude hiding (sum)
import           Unit.Time

runProfile :: ProfileC m a -> m (Timings, a)
runProfile (ProfileC m) = runWriter m

newtype ProfileC m a = ProfileC { runProfileC :: WriterC Timings m a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance (Has (Lift IO) sig m, Effect sig) => Algebra (Profile :+: sig) (ProfileC m) where
  alg = \case
    L (Measure l m k) -> do
      start <- sendM getCurrentTime
      (sub, a) <- ProfileC (censor @Timings (const mempty) (listen (runProfileC m)))
      end <- sendM getCurrentTime
      ProfileC (tell (timing l (end `diffUTCTime` start) sub))
      k a
    R other -> ProfileC (send (handleCoercible other))
    where
    timing ls t = Timings . Map.singleton ls . Timing t t t 1


data Timing = Timing
  { sum   :: !NominalDiffTime
  , min'  :: !NominalDiffTime
  , max'  :: !NominalDiffTime
  , count :: {-# UNPACK #-} !Int
  , sub   :: !Timings
  }

instance Semigroup Timing where
  Timing s1 mn1 mx1 c1 sb1 <> Timing s2 mn2 mx2 c2 sb2 = Timing (s1 + s2) (mn1 `min` mn2) (mx1 `max` mx2) (c1 + c2) (sb1 <> sb2)

instance Monoid Timing where
  mempty = Timing 0 0 0 0 mempty

renderTiming :: Timing -> Doc AnsiStyle
renderTiming t@Timing{ min', max', sub } = table (map go fields) <> if null (unTimings sub) then mempty else nest 2 (line <> renderTimings sub)
    where
    table = group . encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", "
    fields =
      [ (annotate (color Green) "min", prettyMS min')
      , (annotate (color Green) "mean", prettyMS (mean t))
      , (annotate (color Green) "max", prettyMS max')
      ]
    go (k, v) = k <> colon <+> v
    prettyMS = pretty . ($ "ms") . showFFloat (Just 3) . getSeconds . getMilli . milli @Seconds @Double . realToFrac

mean :: Timing -> NominalDiffTime
mean Timing{ sum, count } = sum / fromIntegral count


newtype Timings = Timings { unTimings :: Map.Map Text Timing }

instance Semigroup Timings where
  Timings t1 <> Timings t2 = Timings (Map.unionWith (<>) t1 t2)

instance Monoid Timings where
  mempty = Timings mempty

renderTimings :: Timings -> Doc AnsiStyle
renderTimings (Timings ts) = vsep (map go (sortOn (Down . mean . snd) (Map.toList ts))) where
  go (k, v) = annotate (color Green) (pretty k) <> pretty ':' <> softline <> renderTiming v
