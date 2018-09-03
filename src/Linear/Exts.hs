module Linear.Exts where

import Linear.Matrix
import Linear.V2
import Linear.V3

translated :: V2 Float -> M33 Float
translated (V2 tx ty) = V3 (V3 1 0 tx)
                           (V3 0 1 ty)
                           (V3 0 0 1)
