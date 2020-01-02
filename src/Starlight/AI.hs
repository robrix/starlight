{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Starlight.AI
( ai
) where

import           Control.Effect.Trace
import           Data.Functor.Interval
import qualified Data.Set as Set
import           Linear.Exts
import           Starlight.Action
import           Starlight.Actor as Actor
import           Starlight.Body as Body
import           Starlight.System as System

ai
  :: Has Trace sig m
  => System StateVectors Float
  -> Actor Float
  -> m (Set.Set Action)
ai system Actor{ target, position = P here, rotation } = case target >>= (system !?) of
  -- FIXME: different kinds of behaviours: aggressive, patrolling, mining, trading, etc.
  -- FIXME: don’t just fly directly at the target at full throttle, dumbass
  -- FIXME: factor in the target’s velocity & distance
  -- FIXME: allow other behaviours relating to targets, e.g. following
  Just StateVectors{ position = P there } -> pure . Set.fromList $ concat
    [ [ Face Target ]
    , [ Thrust | isFacing there ]
    ]
  -- FIXME: wander
  -- FIXME: pick a new target
  Nothing -> pure mempty
  where
  isFacing there = abs (wrap (Interval (-pi) pi) (snd (toAxisAngle rotation) - angleTo here there)) < pi/2
