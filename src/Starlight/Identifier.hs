module Starlight.Identifier
( Code
, Identifier(..)
) where

import Data.Text

type Code = Int

data Identifier
  = Star Code Text
