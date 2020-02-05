{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.System
( System(..)
, bodies_
, player_
, players_
, npcs_
, characters_
, beams
, identifiers
, (!?)
, neighbourhoodOf
, neighbourhoodOfPlayer
) where

import           Control.Effect.Lens.Exts (asserting)
import           Control.Lens
import           Data.Either (partitionEithers)
import           Data.Generics.Product.Fields
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           Linear.Exts (toAxisAngle)
import           Starlight.Actor
import           Starlight.Character
import           Starlight.Identifier
import           Starlight.Physics
import           Starlight.Radar
import           Starlight.Ship (radar_)
import           Starlight.Weapon.Laser
import           UI.Colour
import           Unit.Algebra
import           Unit.Length
import           Unit.Power

data System a = System
  { bodies  :: !(Map.Map BodyIdentifier a)
  , players :: !(Map.Map (Code, Name) Character)
  , npcs    :: !(Map.Map (Code, Name) Character)
  }
  deriving (Generic, Show)

bodies_ :: Lens (System a) (System b) (Map.Map BodyIdentifier a) (Map.Map BodyIdentifier b)
bodies_ = field @"bodies"

player_ :: HasCallStack => Lens' (System a) Character
player_ = characters_.at (Player (0, "you")).iso (fromMaybe (error "player missing")) Just

players_ :: Lens' (System a) (Map.Map (Code, Name) Character)
players_ = field @"players"

npcs_ :: Lens' (System a) (Map.Map (Code, Name) Character)
npcs_ = field @"npcs"

characters_ :: HasCallStack => Lens' (System a) (Map.Map CharacterIdentifier Character)
characters_ = lens get set.asserting (Map.member (Player (0, "you"))) where
  get System{ players, npcs } = Map.mapKeys Player players <> Map.mapKeys NPC npcs
  set s cs = s{ players = Map.fromList p, npcs = Map.fromList n } where
    (p, n) = partitionEithers (map (\case{ (Player k, v) -> Left (k, v) ; (NPC k, v) -> Right (k, v) }) (Map.toList cs))

beams :: System a -> [Beam]
beams = toListOf (characters_.itraversed.filtered (view (actions_.contains (Fire Main))).withIndex.to beam) where
  beam (i, c) = Beam{ position = c^.position_, angle = snd (toAxisAngle (c^.rotation_)), colour = green, firedBy = i }


identifiers :: System a -> [Identifier]
identifiers System{ bodies, players, npcs } = map (C . Player) (Map.keys players) <> map (C . NPC) (Map.keys npcs) <> map B (Map.keys bodies)

(!?) :: System a -> Identifier -> Maybe (Either a Character)
(!?) s = \case
  B i -> Left  <$> s^?bodies_    .ix i
  C i -> Right <$> s^?characters_.ix i


neighbourhoodOf :: HasActor a => Character -> System a -> System a
neighbourhoodOf c sys@System{ bodies, players, npcs } = sys
  { bodies  = Map.filterWithKey (visible . B)          bodies
  , players = Map.filterWithKey (visible . C . Player) players
  , npcs    = Map.filterWithKey (visible . C . NPC)    npcs
  } where
  -- FIXME: occlusion
  -- FIXME: jamming
  -- FIXME: ghosts
  -- FIXME: doppler effect
  -- FIXME: radar cross-section, rather than just size
  -- FIXME: laser power, not radar power, determines laser range
  -- FIXME: radar reflections
  -- FIXME: sharing radar with allies
  -- FIXME: dimensional analysis
  visible i a = case i of
    B (Star _) -> True
    _          -> received .>. threshold
    where
    received = radarRange (c^.ship_.radar_.power_.converting) gain (convert aperture) (convert crossSection) patternPropagationFactor (convert <$> a^.position_) (convert <$> c^.position_)
    crossSection = a^.magnitude_ .*. a^.magnitude_
  aperture :: (Mega Metres :^: 2) Double
  aperture = 10
  gain = 1
  patternPropagationFactor = 1
  threshold :: Pico Watts Double
  threshold = 1

neighbourhoodOfPlayer :: HasActor a => System a -> System a
neighbourhoodOfPlayer sys = neighbourhoodOf (sys^.player_) sys
