{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Faction
( Factions(..)
, factions
, getFactions
, PFaction(..)
, Faction(..)
, name_
, relationships_
) where

import Control.Lens
import Control.Monad (ap)
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


data PFaction a
  = Var a
  | In (Faction (PFaction a))
  deriving (Functor)

instance Applicative PFaction where
  pure = Var
  (<*>) = ap

instance Monad PFaction where
  Var a >>= f = f a
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
