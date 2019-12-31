module Starlight.Identifier
( Code
, Name(..)
, Identifier(..)
) where

import Data.Text

type Code = Int

newtype Name = Name { getName :: Text }

data Identifier
  = Star Code Name
