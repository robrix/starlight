{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.System
( System(..)
, scale_
, bodies_
, player_
, npcs_
, characters_
, beams_
, systemTrans
, identifiers
, (!?)
, neighbourhoodOf
, neighbourhoodOfPlayer
) where

import           Control.Effect.Lens.Exts (asserting)
import           Control.Lens (Lens, Lens', at, iso, ix, lens, (^.), (^?))
import           Data.Generics.Product.Fields
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           Linear.Exts
import           Starlight.Actor
import           Starlight.Character
import           Starlight.Identifier
import           Starlight.Radar
import           Starlight.Ship (radar_)
import           Starlight.Weapon.Laser
import           Unit.Power

data System a = System
  { scale      :: !Float
  , bodies     :: !(Map.Map BodyIdentifier a)
  , characters :: !(Map.Map CharacterIdentifier Character)
  , beams      :: ![Beam]
  }
  deriving (Generic, Show)

scale_ :: Lens' (System a) Float
scale_ = field @"scale"

bodies_ :: Lens (System a) (System b) (Map.Map BodyIdentifier a) (Map.Map BodyIdentifier b)
bodies_ = field @"bodies"

player_ :: HasCallStack => Lens' (System a) Character
player_ = characters_.at Player .iso (fromMaybe (error "player missing")) Just

npcs_ :: Lens' (System a) [Character]
npcs_ = characters_.lens (Map.elems . Map.delete Player) (\ m cs -> Map.fromList ((Player, m Map.! Player) : zipWith ((,) . NPC) [0..] cs))

characters_ :: HasCallStack => Lens' (System a) (Map.Map CharacterIdentifier Character)
characters_ = field @"characters".asserting (Map.member Player)

beams_ :: Lens' (System a) [Beam]
beams_ = field @"beams"


systemTrans :: System a -> M44 Float
systemTrans System{ scale } = scaled (V4 scale scale scale 1)

identifiers :: System a -> [Identifier]
identifiers System{ bodies, characters } = map C (Map.keys characters) <> map B (Map.keys bodies)

(!?) :: System a -> Identifier -> Maybe (Either a Character)
(!?) s = \case
  B i -> Left  <$> s^?bodies_    .ix i
  C i -> Right <$> s^?characters_.ix i


neighbourhoodOf :: HasActor a => Character -> System a -> System a
neighbourhoodOf c sys@System{ bodies, characters } = sys
  { bodies     = Map.filterWithKey (visible . B) bodies
  , characters = Map.filterWithKey (visible . C) characters
  } where
  -- FIXME: occlusion
  -- FIXME: jamming
  -- FIXME: ghosts
  -- FIXME: doppler effect
  -- FIXME: radar cross-section, rather than just size
  -- FIXME: laser power, not radar power, determines laser range
  -- FIXME: radar reflections
  -- FIXME: sharing radar with allies
  visible i a = case i of
    B (Star _) -> True
    _          -> received > threshold
    where
    r = qd (a^.actor_.position_) (c^.actor_.position_)
    received = Watts ((c^.ship_.radar_.power_.unitary * gain * aperture * prj (a^.actor_.magnitude_) * patternPropagationFactor ** 4) / ((4 * pi) ** 2 * prj (r ** 2)))
  patternPropagationFactor = 1
  gain = 1000000
  aperture = 1000000
  threshold = Watts 1.0e-12 -- 1 picowatt

neighbourhoodOfPlayer :: HasActor a => System a -> System a
neighbourhoodOfPlayer sys = neighbourhoodOf (sys^.player_) sys
