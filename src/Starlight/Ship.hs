{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Ship
( Ship(..)
, colour_
, health_
) where

import Control.Lens (Lens')
import Data.Generics.Product.Fields
import GHC.Generics (Generic)
import UI.Colour

data Ship = Ship
  { colour :: Colour Float
  , health :: Float
  }
  deriving (Generic, Show)

colour_ :: Lens' Ship (Colour Float)
colour_ = field @"colour"

health_ :: Lens' Ship Float
health_ = field @"health"
