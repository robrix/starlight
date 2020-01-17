{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Ship
( Ship(..)
, colour_
, armour_
, radar_
) where

import Control.Lens (Lens')
import Data.Functor.I
import Data.Functor.Interval
import Data.Generics.Product.Fields
import GHC.Generics (Generic)
import Starlight.Radar
import UI.Colour

data Ship = Ship
  { colour :: Colour Float
  , armour :: Interval I Double
  , radar  :: Radar
  }
  deriving (Generic, Show)

colour_ :: Lens' Ship (Colour Float)
colour_ = field @"colour"

armour_ :: Lens' Ship (Interval I Double)
armour_ = field @"armour"

radar_ :: Lens' Ship Radar
radar_ = field @"radar"
