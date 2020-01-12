{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import           Unit

ai
  :: Has (Reader (System StateVectors)) sig m
  => Character
  -> m Character
ai c@Character{ actor = Actor{ position = here, rotation }, target } = do
  system <- ask
  pure $! c & actions_ .~ case target >>= (system !?) of
    -- FIXME: different kinds of behaviours: aggressive, patrolling, mining, trading, etc.
    -- FIXME: don’t just fly directly at the target at full throttle, dumbass
    -- FIXME: factor in the target’s velocity & distance
    -- FIXME: allow other behaviours relating to targets, e.g. following
    Just (Left StateVectors{ actor = Actor{ position = there } }) -> Set.fromList $ concat
      [ [ Face Target ]
      , [ Thrust | isFacing (pi/4) rotation (angleTo' there) ]
      ]
    Just (Right Character{ actor = Actor{ position = there } }) -> Set.fromList $ concat
      [ [ Face Target ]
      , [ Thrust    | isFacing (pi/4)   rotation (angleTo' there) ]
      , [ Fire Main | isFacing (pi/128) rotation (angleTo' there) ]
      ]
    -- FIXME: wander
    -- FIXME: pick a new target
    _ -> mempty
    where
    angleTo' there = prj <$> angleTo (here^._xy) (there^._xy)
