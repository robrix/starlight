{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.Main
( main
, Lifts
) where

import           Control.Algebra
import qualified Control.Carrier.Profile.Identity as NoProfile
import qualified Control.Carrier.Profile.Tree as Profile
import           Control.Carrier.Reader
import           Control.Carrier.Thread.IO
import qualified Control.Carrier.Trace.Ignoring as NoTrace
import qualified Control.Carrier.Trace.Lift as Trace
import           Control.Effect.Lens.Exts as Lens
import           Control.Effect.Profile
import           Control.Effect.Trace
import           Control.Monad.Fix
import           Control.Monad.IO.Class.Lift
import           Data.Bool (bool)
import           Data.Flag
import           Data.Kind (Constraint)
import qualified GL.Carrier.Check.Identity as NoCheck
import qualified GL.Carrier.Check.IO as Check
import           GL.Effect.Check
import qualified Starlight.CLI as CLI
import           Starlight.Game

main :: IO ()
main = do
  options <- CLI.execParser CLI.argumentsParser
  runThread (runReader options (runCheck (runProfile (CLI.profile options) (runTrace game))))

runProfile
  :: ( Effect sig
     , Has (Lift IO) sig m
     )
  => Flag CLI.ShouldProfile
  -> (forall t . (Lifts MonadFail t, Lifts MonadFix t, Lifts MonadIO t, Algebra (Profile :+: sig) (t m)) => t m a)
  -> m a
runProfile flag
  | fromFlag CLI.ShouldProfile flag = Profile.reportProfile
  | otherwise                       = NoProfile.runProfile

runTrace
  :: ( Has (Lift IO) sig m
     , Has (Reader CLI.Options) sig m
     )
  => (forall t . (Lifts MonadFail t, Lifts MonadFix t, Lifts MonadIO t, Algebra (Trace :+: sig) (t m)) => t m a)
  -> m a
runTrace m = view CLI.trace_ >>= bool (NoTrace.runTrace m) (Trace.runTrace m) . fromFlag CLI.ShouldTrace

runCheck
  :: ( Has (Lift IO) sig m
     , Has (Reader CLI.Options) sig m
     )
  => (forall t . (Lifts MonadFail t, Lifts MonadFix t, Lifts MonadIO t, Algebra (Check :+: sig) (t m)) => t m a)
  -> m a
runCheck m = view CLI.check_ >>= bool (NoCheck.runCheck m) (Check.runCheck m) . fromFlag CLI.ShouldCheck

type Lifts (c :: (* -> *) -> Constraint) t = ((forall m' . c m' => c (t m')) :: Constraint)
