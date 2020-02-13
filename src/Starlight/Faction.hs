{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
module Starlight.Faction
( Factions(..)
, Faction(..)
) where

import Bound.Scope
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import UI.Colour

newtype Factions = Factions { getFactions :: [Scope Int Faction Void] }

data PFaction a b
  = Var b
  | Mu (a -> PFaction a b)
  | In (Faction b)
  deriving (Functor)

data Faction a = Faction
  { name          :: Text
  , colour        :: Colour Float
  , relationships :: [(a, Float)]
  }
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

instance HasColour (Faction a)
