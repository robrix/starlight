{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Ship
( Ship(..)
) where

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
