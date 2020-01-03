{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Starlight.AI
( ai
) where

import           Control.Effect.Lens
import           Control.Effect.Reader
import           Control.Effect.State
import           Data.Functor.Interval
import qualified Data.Set as Set
import           Linear.Exts
import           Starlight.Action
import           Starlight.Actor as Actor
import           Starlight.Body as Body
import           Starlight.System as System

ai
  :: ( Has (Reader (System StateVectors)) sig m
     , Has (State Actor) sig m
     )
  => m ()
ai = go <$> ask <*> get >>= assign actions_ where
  go system Actor{ target, position = P here, rotation } = case target >>= (system !?) of
    -- FIXME: different kinds of behaviours: aggressive, patrolling, mining, trading, etc.
    -- FIXME: don’t just fly directly at the target at full throttle, dumbass
    -- FIXME: factor in the target’s velocity & distance
    -- FIXME: allow other behaviours relating to targets, e.g. following
    Just (Left StateVectors{ position = P there }) -> Set.fromList $ concat
      [ [ Face Target ]
      , [ Thrust | isFacing there ]
      ]
    Just (Right Actor{ position = P there }) -> Set.fromList $ concat
      [ [ Face Target ]
      , [ Thrust | isFacing there ]
      ]
    -- FIXME: wander
    -- FIXME: pick a new target
    _ -> mempty
    where
    isFacing there = abs (wrap (Interval (-pi) pi) (snd (toAxisAngle rotation) - angleTo here there)) < pi/2
