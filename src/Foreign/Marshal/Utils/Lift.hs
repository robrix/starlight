module Foreign.Marshal.Utils.Lift
( with
) where

import           Control.Carrier.Lift
import qualified Foreign.Marshal.Utils as U
import           Foreign.Ptr
import           Foreign.Storable

with :: (Has (Lift IO) sig m, Storable a) => a -> (Ptr a -> m b) -> m b
with a action = liftWith $ \ hdl ctx -> U.with a (hdl . (<$ ctx) . action)
