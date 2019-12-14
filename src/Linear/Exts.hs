module Linear.Exts
( translated
, rotated
, scaled
) where

import Linear.Matrix
import Linear.V2
import Linear.V3
import Linear.Vector

translated :: V2 Float -> M33 Float
translated (V2 tx ty) = V3
  (V3 1 0 tx)
  (V3 0 1 ty)
  (V3 0 0 1)

rotated :: Float -> M33 Float
rotated theta = V3
  (V3 cosT (-sinT) 0)
  (V3 sinT cosT    0)
  (V3 0    0       1) where
  cosT = cos theta
  sinT = sin theta
