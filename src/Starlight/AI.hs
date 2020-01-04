{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Starlight.AI
( ai
) where

import           Control.Effect.Reader
import           Control.Lens (set, (^.))
import           Data.Functor.Interval
import qualified Data.Set as Set
import           Linear.Exts
import           Starlight.Action
import           Starlight.Actor as Actor
import           Starlight.Body as Body
import           Starlight.Character
import           Starlight.System as System

ai
  :: Has (Reader (System StateVectors)) sig m
  => Character
  -> m Character
ai c@Character{ actor = Actor{ position = P here, rotation }, target } = asks (flip (set actions_) c . go) where
  go system = case target >>= (system !?) of
    -- FIXME: different kinds of behaviours: aggressive, patrolling, mining, trading, etc.
    -- FIXME: don’t just fly directly at the target at full throttle, dumbass
    -- FIXME: factor in the target’s velocity & distance
    -- FIXME: allow other behaviours relating to targets, e.g. following
    Just (Left StateVectors{ actor = Actor{ position = P there } }) -> Set.fromList $ concat
      [ [ Face Target ]
      , [ Thrust | isFacing (pi/4) there ]
      ]
    Just (Right Character{ actor = Actor{ position = P there } }) -> Set.fromList $ concat
      [ [ Face Target ]
      , [ Thrust    | isFacing (pi/4)   there ]
      , [ Fire Main | isFacing (pi/128) there ]
      ]
    -- FIXME: wander
    -- FIXME: pick a new target
    _ -> mempty
    where
    isFacing epsilon there = abs (wrap (Interval (-pi) pi) (snd (toAxisAngle rotation) - angleTo (here ^. _xy) (there ^. _xy))) < epsilon
