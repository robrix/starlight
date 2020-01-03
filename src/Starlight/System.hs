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
, characters_
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
  { scale      :: !Float
  , bodies     :: !(Map.Map BodyIdentifier a)
  , characters :: ![Character]
  }
  deriving (Generic, Show)

systemTrans :: System a -> M44 Float
systemTrans System{ scale } = scaled (V4 scale scale scale 1)

scale_ :: Lens' (System a) Float
scale_ = field @"scale"

bodies_ :: Lens (System a) (System b) (Map.Map BodyIdentifier a) (Map.Map BodyIdentifier b)
bodies_ = field @"bodies"

characters_ :: Lens' (System a) [Character]
characters_ = field @"characters"

identifiers :: System a -> [Identifier]
identifiers System{ bodies, characters } = Player:map S [0..pred (length characters)] <> map B (Map.keys bodies)

(!?) :: System a -> Identifier -> Maybe (Either a Character)
(!?) System{ bodies, characters } = \case
  B i    -> Left  <$> Map.lookup i bodies
  S i    -> Right <$> characters ^? ix i
  Player -> Nothing
