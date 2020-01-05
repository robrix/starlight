{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Ship
( Ship(..)
, colour_
) where

import Control.Lens (Lens')
import Data.Generics.Product.Fields
import GHC.Generics (Generic)
import Starlight.Actor
import UI.Colour

data Ship = Ship
  { colour :: Colour Float
  , actor  :: Actor
  , health :: Float
  }
  deriving (Generic, Show)

instance HasActor Ship where
  actor_ = field @"actor"


colour_ :: Lens' Ship (Colour Float)
colour_ = field @"colour"
