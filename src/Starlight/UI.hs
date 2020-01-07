module Starlight.UI
( UI(..)
) where

import UI.Label
import UI.Typeface

data UI = UI
  { fps    :: Label
  , target :: Label
  , face   :: Typeface
  }
