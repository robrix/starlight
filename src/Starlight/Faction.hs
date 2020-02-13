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
  In f  <*> a = In (f & relationships_.traversed._1 %~ (<*> a))

instance Monad (PFaction a) where
  Var a >>= f = f a
  Mu a  >>= f = Mu ((>>= f) . a)
  In a  >>= f = In (a & relationships_.traversed._1 %~ (>>= f))

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
