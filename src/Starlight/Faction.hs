{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
module Starlight.Faction
( Factions(..)
, PFaction(..)
, Faction(..)
) where

import Data.Text (Text)
import GHC.Generics (Generic)
import UI.Colour

newtype Factions = Factions { getFactions :: forall v . [PFaction v v] }

data PFaction a b
  = Var b
  | Mu (a -> PFaction a b)
  | In (Faction (PFaction a b))
  deriving (Functor)

data Faction a = Faction
  { name          :: Text
  , colour        :: Colour Float
  , relationships :: [(a, Float)]
  }
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

instance HasColour (Faction a)
