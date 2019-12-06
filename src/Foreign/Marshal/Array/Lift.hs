module Foreign.Marshal.Array.Lift
( allocaArray
) where

import Control.Carrier.Lift
import qualified Foreign.Marshal.Array as A
import Foreign.Ptr
import Foreign.Storable

allocaArray :: (Has (Lift IO) sig m, Storable a) => Int -> (Ptr a -> m b) -> m b
allocaArray n with = liftWith $ \ ctx hdl -> A.allocaArray n (hdl . (<$ ctx) . with)
