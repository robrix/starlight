module UI.Label
( Label(..)
) where

import Lens.Micro
import UI.Colour
import UI.Font

data Label = Label
  { font   :: !Font
  , text   :: !String
  , colour :: !(Colour Float)
  }

_font :: Lens' Label Font
_font = lens font (\ l f -> l { font = f })

_text :: Lens' Label String
_text = lens text (\ l t -> l { text = t })

_colour :: Lens' Label (Colour Float)
_colour = lens colour (\ l c -> l { colour = c })
