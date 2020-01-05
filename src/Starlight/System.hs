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
) where

import           Control.Lens (Lens, Lens', ix, lens, (&), (.~), (^.), (^?))
import           Data.Generics.Product.Fields
import qualified Data.Map.Strict as Map
import           GHC.Generics (Generic)
import           Linear.Matrix
import           Linear.V4
import           Linear.Vector
import           Starlight.Character
import           Starlight.Draw.Weapon.Laser
import           Starlight.Identifier

data System a = System
  { scale  :: !Float
  , bodies :: !(Map.Map BodyIdentifier a)
  , player :: !Character
  , npcs   :: ![Character]
  , beams  :: ![Beam]
  }
  deriving (Generic, Show)

scale_ :: Lens' (System a) Float
scale_ = field @"scale"

bodies_ :: Lens (System a) (System b) (Map.Map BodyIdentifier a) (Map.Map BodyIdentifier b)
bodies_ = field @"bodies"

player_ :: Lens' (System a) Character
player_ = field @"player"

npcs_ :: Lens' (System a) [Character]
npcs_ = field @"npcs"

characters_ :: Lens' (System a) (Map.Map CharacterIdentifier Character)
characters_ = lens get set where
  get s = Map.fromList ((Player, s^.player_) : zipWith ((,) . NPC) [0..] (s^.npcs_))
  set s m = s & player_ .~ (m Map.! Player) & npcs_ .~ Map.elems (Map.delete Player m)

beams_ :: Lens' (System a) [Beam]
beams_ = field @"beams"


systemTrans :: System a -> M44 Float
systemTrans System{ scale } = scaled (V4 scale scale scale 1)

identifiers :: System a -> [Identifier]
identifiers System{ bodies, npcs } = C Player : map (C . NPC) [0..pred (length npcs)] <> map B (Map.keys bodies)

(!?) :: System a -> Identifier -> Maybe (Either a Character)
(!?) System{ bodies, player, npcs } = \case
  B      i  -> Left  <$> Map.lookup i bodies
  C (NPC i) -> Right <$> npcs ^? ix i
  C Player  -> Just (Right player)
