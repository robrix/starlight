{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Faction
( Factions(..)
, PFaction(..)
, Faction(..)
, name_
, relationships_
) where

import Control.Lens
import Data.Bifunctor (first)
import Data.Generics.Product.Fields
import Data.Text (Text)
import GHC.Generics (Generic)
import UI.Colour

newtype Factions = Factions { getFactions :: forall v . [PFaction v v] }

data PFaction a b
  = Var b
  | Mu (a -> PFaction a b)
  | In (Faction (PFaction a b))
  deriving (Functor)

instance Applicative (PFaction a) where
  pure = Var
  Var f <*> a = f <$> a
  Mu f  <*> a = Mu ((<*> a) . f)
  In f  <*> a = In (Faction (name f) (colour f) (map (first (<*> a)) (relationships f)))

data Faction a = Faction
  { name          :: Text
  , colour        :: Colour Float
  , relationships :: [(a, Float)]
  }
  deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

instance HasColour (Faction a)

name_ :: Lens' (Faction a) Text
name_ = field @"name"

relationships_ :: Lens (Faction a) (Faction b) [(a, Float)] [(b, Float)]
relationships_ = field @"relationships"
