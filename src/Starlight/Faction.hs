{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Faction
( Factions
, factions
, getFactions
, Faction(..)
, name_
, relationships_
) where

import Control.Lens
import Data.Generics.Product.Fields
import Data.IntMap as IntMap
import Data.Text (Text)
import GHC.Generics (Generic)
import UI.Colour

newtype Factions = Factions (forall v . [v] -> [Faction v])

factions :: IntMap (Faction Int) -> Factions
factions fs = Factions (\ vs -> go vs <$> IntMap.elems fs) where
  go vs f = f & relationships_.traversed._1 %~ (vs !!)

getFactions :: Factions -> IntMap (Faction Int)
getFactions (Factions fs) = IntMap.fromList (zip [0..] (fs [0..]))


data Faction a = Faction
  { name          :: Text
  , colour        :: Colour Float
  , relationships :: [(a, Double)]
  }
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

instance HasColour (Faction a)

name_ :: Lens' (Faction a) Text
name_ = field @"name"

relationships_ :: Lens (Faction a) (Faction b) [(a, Double)] [(b, Double)]
relationships_ = field @"relationships"
