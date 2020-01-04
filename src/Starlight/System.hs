{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.System
( System(..)
, systemTrans
, scale_
, bodies_
, player_
, npcs_
, identifiers
, (!?)
) where

import           Control.Lens (Lens, Lens', ix, (^?))
import           Data.Generics.Product.Fields
import qualified Data.Map as Map
import           GHC.Generics (Generic)
import           Linear.Matrix
import           Linear.V4
import           Linear.Vector
import           Starlight.Character
import           Starlight.Identifier

data System a = System
  { scale  :: !Float
  , bodies :: !(Map.Map BodyIdentifier a)
  , player :: !Character
  , npcs   :: ![Character]
  }
  deriving (Generic, Show)

systemTrans :: System a -> M44 Float
systemTrans System{ scale } = scaled (V4 scale scale scale 1)

scale_ :: Lens' (System a) Float
scale_ = field @"scale"

bodies_ :: Lens (System a) (System b) (Map.Map BodyIdentifier a) (Map.Map BodyIdentifier b)
bodies_ = field @"bodies"

player_ :: Lens' (System a) Character
player_ = field @"player"

npcs_ :: Lens' (System a) [Character]
npcs_ = field @"npcs"

identifiers :: System a -> [Identifier]
identifiers System{ bodies, npcs } = Player : map S [0..pred (length npcs)] <> map B (Map.keys bodies)

(!?) :: System a -> Identifier -> Maybe (Either a Character)
(!?) System{ bodies, player, npcs } = \case
  B i    -> Left  <$> Map.lookup i bodies
  S i    -> Right <$> npcs ^? ix i
  Player -> Just (Right player)
