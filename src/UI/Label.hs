module UI.Label
( Label(..)
) where

import UI.Font

data Label = Label
  { font :: !Font
  , text :: !String
  }
