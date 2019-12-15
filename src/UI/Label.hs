module UI.Label
( Label(..)
) where

import UI.Colour
import UI.Font

data Label = Label
  { font   :: !Font
  , text   :: !String
  , colour :: !(Colour Float)
  }
