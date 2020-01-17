{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.System
( System(..)
, bodies_
, player_
, npcs_
, characters_
, beams_
, identifiers
, (!?)
, neighbourhoodOf
, neighbourhoodOfPlayer
) where

import           Control.Effect.Lens.Exts (asserting)
import           Control.Lens (Lens, Lens', at, iso, ix, lens, (^.), (^?))
import           Data.Coerce
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
import           Unit.Algebra
import           Unit.Length
import           Unit.Power

data System a = System
  { bodies     :: !(Map.Map BodyIdentifier a)
  , characters :: !(Map.Map CharacterIdentifier Character)
  , beams      :: ![Beam]
  }
  deriving (Generic, Show)

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
  -- FIXME: dimensional analysis
  visible i a = case i of
    B (Star _) -> True
    _          -> received > threshold
    where
    received :: Pico Watts Double
    received = (c^.ship_.radar_.power_.convertingTo (Pico . Watts) .*. gain .*. aperture .*. crossSection .*. patternPropagationFactor ** 4) ./. (I ((4 * pi) ** 2) .*. r .*. r)
    crossSection :: (Mega Metres :*: Mega Metres) Double
    crossSection = a^.actor_.magnitude_ .*. a^.actor_.magnitude_
    r :: (Mega Metres :*: Mega Metres) Double
    r = coerce $ (a^.actor_.position_) `qd` (c^.actor_.position_)
  aperture :: (Mega Metres :*: Mega Metres) Double
  aperture = 10
  gain :: I Double
  gain = 1
  patternPropagationFactor :: I Double
  patternPropagationFactor = 1
  threshold :: Pico Watts Double
  threshold = 1

neighbourhoodOfPlayer :: HasActor a => System a -> System a
neighbourhoodOfPlayer sys = neighbourhoodOf (sys^.player_) sys
