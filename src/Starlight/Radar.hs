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
  { power :: Mega Watts Float
  }
  deriving (Generic, Show)

power_ :: Lens' Radar (Mega Watts Float)
power_ = field @"power"
