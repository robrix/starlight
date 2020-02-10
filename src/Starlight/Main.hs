{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.Main
( main
, Lifts
) where

import           Control.Algebra
import qualified Control.Carrier.Profile.Identity as NoProfile
import qualified Control.Carrier.Profile.Tree as Profile
import           Control.Carrier.Thread.IO
import qualified Control.Carrier.Trace.Ignoring as NoTrace
import qualified Control.Carrier.Trace.Lift as Trace
import           Control.Effect.Profile
import           Control.Effect.Trace
import           Control.Monad.Fix
import           Control.Monad.IO.Class.Lift
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
  runThread (runCheck (CLI.check options) (runProfile (CLI.profile options) (runTrace (CLI.trace options) game)))

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
  :: Has (Lift IO) sig m
  => Flag CLI.ShouldTrace
  -> (forall t . (Lifts MonadFail t, Lifts MonadFix t, Lifts MonadIO t, Algebra (Trace :+: sig) (t m)) => t m a)
  -> m a
runTrace flag
  | fromFlag CLI.ShouldTrace flag = Trace.runTrace
  | otherwise                     = NoTrace.runTrace

runCheck
  :: Has (Lift IO) sig m
  => Flag CLI.ShouldCheck
  -> (forall t . (Lifts MonadFail t, Lifts MonadFix t, Lifts MonadIO t, Algebra (Check :+: sig) (t m)) => t m a)
  -> m a
runCheck flag
  | fromFlag CLI.ShouldCheck flag = Check.runCheck
  | otherwise                     = NoCheck.runCheck

type Lifts (c :: (* -> *) -> Constraint) t = ((forall m' . c m' => c (t m')) :: Constraint)
