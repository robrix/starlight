module Starlight.UI
( UI(..)
) where

import UI.Label

data UI = UI
  { fps    :: Label
  , target :: Label
  }
