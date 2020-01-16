{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Radar
( Radar(..)
, power_
) where

import Control.Lens
import Data.Generics.Product.Fields
import GHC.Generics (Generic)
import Unit.Power

-- FIXME: transmitter gain
-- FIXME: effective aperture
newtype Radar = Radar
  { power :: Mega Watts Double
  }
  deriving (Generic, Show)

power_ :: Lens' Radar (Mega Watts Double)
power_ = field @"power"
