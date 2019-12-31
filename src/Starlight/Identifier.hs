module Starlight.Identifier
( Identifier(..)
) where

import Data.Text

data Identifier
  = Star Text Int
