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
import           Control.Lens (Lens, Lens', at, imap, iso, ix, lens, to, (^.), (^?))
import           Data.Bifunctor (first)
import           Data.Generics.Product.Fields
import           Data.List (partition)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           Starlight.Actor
import           Starlight.Character
import           Starlight.Identifier
import           Starlight.Physics.Constants
import           Starlight.Radar
import           Starlight.Ship (radar_)
import           Starlight.Weapon.Laser
import           Unit.Algebra
import           Unit.Length
import           Unit.Power

data System a = System
  { bodies  :: !(Map.Map BodyIdentifier a)
  , players :: ![Character]
  , npcs    :: ![Character]
  , beams   :: ![Beam]
  }
  deriving (Generic, Show)

bodies_ :: Lens (System a) (System b) (Map.Map BodyIdentifier a) (Map.Map BodyIdentifier b)
bodies_ = field @"bodies"

player_ :: HasCallStack => Lens' (System a) Character
player_ = characters_.at (Player 0).iso (fromMaybe (error "player missing")) Just

players_ :: Lens' (System a) [Character]
players_ = field @"players"

npcs_ :: Lens' (System a) [Character]
npcs_ = field @"npcs"

characters_ :: HasCallStack => Lens' (System a) (Map.Map CharacterIdentifier Character)
characters_ = lens get set.asserting (Map.member (Player 0)) where
  get s = Map.fromList (s^.players_.to (imap ((,) . Player)) <> s^.npcs_.to (imap ((,) . NPC)))
  set s cs = s{ players = map snd p, npcs = map snd n } where
    (p, n) = partition (\case{ Player _ -> True ; _ -> False } . fst) (Map.toList cs)

beams_ :: Lens' (System a) [Beam]
beams_ = field @"beams"


identifiers :: System a -> [Identifier]
identifiers System{ bodies, players, npcs } = imap (const . C . Player) players <> imap (const . C . NPC) npcs <> map B (Map.keys bodies)

(!?) :: System a -> Identifier -> Maybe (Either a Character)
(!?) s = \case
  B i -> Left  <$> s^?bodies_    .ix i
  C i -> Right <$> s^?characters_.ix i


neighbourhoodOf :: HasActor a => Character -> System a -> System a
neighbourhoodOf c sys@System{ bodies, players, npcs } = sys
  { bodies  = Map.filterWithKey (visible . B) bodies
  , players = map snd (filter (uncurry visible . first C) (zipWith ((,) . Player) [0..] players))
  , npcs    = map snd (filter (uncurry visible . first C) (zipWith ((,) . NPC)    [0..] npcs))
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
    received = (c^.ship_.radar_.power_.convertingTo (Pico . Watts) .*. gain .*. convert @(Mega Metres :^: 2) @(Distance :^: 2) aperture .*. crossSection .*. patternPropagationFactor ** 4) ./. (I ((4 * pi) ** 2) .*. r .*. r)
    crossSection :: (Distance :^: 2) Double
    crossSection = a^.magnitude_ .*. a^.magnitude_
    r :: (Distance :^: 2) Double
    r = (a^.position_) `qdU` (c^.position_)
  aperture :: (Mega Metres :^: 2) Double
  aperture = 10
  gain :: I Double
  gain = 1
  patternPropagationFactor :: I Double
  patternPropagationFactor = 1
  threshold :: Pico Watts Double
  threshold = 1

neighbourhoodOfPlayer :: HasActor a => System a -> System a
neighbourhoodOfPlayer sys = neighbourhoodOf (sys^.player_) sys
