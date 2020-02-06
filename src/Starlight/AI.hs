{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.AI
( ai
) where

import           Control.Effect.Reader
import           Control.Lens ((&), (.~), (^.))
import qualified Data.Set as Set
import           Linear.Exts
import           Starlight.Actor as Actor
import           Starlight.Body as Body
import           Starlight.Character
import           Starlight.System as System

ai
  :: Has (Reader (System StateVectors)) sig m
  => Character
  -> m Character
ai c@Character{ actor = Actor{ position = here, rotation }, target } = do
  system <- ask @(System StateVectors)
  pure $! c & actions_ .~ case target >>= (system !?) of
    -- FIXME: different kinds of behaviours: aggressive, patrolling, mining, trading, etc.
    -- FIXME: don’t just fly directly at the target at full throttle, dumbass
    -- FIXME: factor in the target’s velocity & distance
    -- FIXME: allow other behaviours relating to targets, e.g. following
    Just (Left sv) -> Set.fromList
      ( Face Target
      : [ Thrust | facingRel rotation (angleTo' (sv^.position_)) < pi/4 ]
      )
    Just (Right c) -> Set.fromList $ concat
      [ [ Face Target ]
      , [ Thrust    | facingRel rotation (angleTo' (c^.position_)) < pi/4 ]
      , [ Fire Main | facingRel rotation (angleTo' (c^.position_)) < pi/128 ]
      ]
    -- FIXME: wander
    -- FIXME: pick a new target
    _ -> mempty
    where
    angleTo' there = angleTo (here^._xy) (there^._xy)
