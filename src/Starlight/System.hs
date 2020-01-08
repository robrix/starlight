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

import           Control.Lens (Lens, Lens', ix, lens, (^.), (^?))
import           Data.Generics.Product.Fields
import qualified Data.Map.Strict as Map
import           GHC.Generics (Generic)
import           Linear.Exts
import           Starlight.Actor
import           Starlight.Character
import           Starlight.Identifier
import           Starlight.Weapon.Laser
import           Unit.Length

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

player_ :: Lens' (System a) Character
player_ = characters_.lens (Map.! Player) (flip (Map.insert Player))

npcs_ :: Lens' (System a) [Character]
npcs_ = characters_.lens (Map.elems . Map.delete Player) (\ m cs -> Map.fromList ((Player, m Map.! Player) : zipWith ((,) . NPC) [0..] cs))

characters_ :: Lens' (System a) (Map.Map CharacterIdentifier Character)
characters_ = field @"characters"

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


neighbourhoodOf :: HasActor a => Character -> Kilo Metres Float -> System a -> System a
neighbourhoodOf c (Kilo (Metres r)) sys@System{ bodies, characters } = sys{ bodies = Map.filter visible bodies, characters = Map.filter visible characters } where
  -- FIXME: occlusion
  -- FIXME: always include first- and second-order bodies
  -- FIXME: compute received radar power
  -- FIXME: larger objects are more observable
  -- FIXME: radar cross-section, rather than just size
  -- FIXME: laser power, not radar power, determines laser range
  -- FIXME: radar reflections
  -- FIXME: sharing radar with allies
  visible a = distance (a^.actor_.position_) (c^.actor_.position_) < r

neighbourhoodOfPlayer :: HasActor a => Kilo Metres Float -> System a -> System a
neighbourhoodOfPlayer r sys = neighbourhoodOf (sys^.player_) r sys
