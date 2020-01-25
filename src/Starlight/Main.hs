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
import qualified Control.Exception.Lift as E
import           Control.Monad.Fix
import           Control.Monad.IO.Class.Lift
import           Data.Bool (bool)
import           Data.Kind (Constraint)
import qualified GL.Carrier.Check.Identity as NoCheck
import qualified GL.Carrier.Check.IO as Check
import           GL.Effect.Check
import qualified Starlight.CLI as CLI
import           Starlight.Game
import           System.Environment
import           System.Exit

main :: IO ()
main = handling $ CLI.execParser CLI.argumentsParser >>= runThread . (`runReader` (runCheck (runProfile (runTrace game))))
  where
  handling m = do
    name <- getProgName
    -- Exceptions don’t seem to exit in the repl for unknown reasons, so we catch and log them (except for 'ExitCode')
    if name == "<interactive>" then
      m `E.catches`
        [ E.Handler (const @_ @ExitCode (pure ()))
        , E.Handler (putStrLn . E.displayException @E.SomeException)
        ]
    else
      m

runProfile
  :: ( Has (Lift IO) sig m
     , Effect sig
     , Has (Reader CLI.Options) sig m
     )
  => (forall t . (Lifts MonadFail t, Lifts MonadFix t, Lifts MonadIO t, Algebra (Profile :+: sig) (t m)) => t m a)
  -> m a
runProfile m = view CLI.profile_ >>= bool (NoProfile.runProfile m) (Profile.reportProfile m)

runTrace
  :: ( Has (Lift IO) sig m
     , Has (Reader CLI.Options) sig m
     )
  => (forall t . (Lifts MonadFail t, Lifts MonadFix t, Lifts MonadIO t, Algebra (Trace :+: sig) (t m)) => t m a)
  -> m a
runTrace m = view CLI.trace_ >>= bool (NoTrace.runTrace m) (Trace.runTrace m)

runCheck
  :: ( Has (Lift IO) sig m
     , Has (Reader CLI.Options) sig m
     )
  => (forall t . (Lifts MonadFail t, Lifts MonadFix t, Lifts MonadIO t, Algebra (Check :+: sig) (t m)) => t m a)
  -> m a
runCheck m = view CLI.trace_ >>= bool (NoCheck.runCheck m) (Check.runCheck m)

type Lifts (c :: (* -> *) -> Constraint) t = (forall m' . c m' => c (t m')) :: Constraint
