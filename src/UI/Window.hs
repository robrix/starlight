module UI.Window where

import Linear.V2 as Linear

data Window = Window String {-# UNPACK #-} !(Linear.V2 Int)
