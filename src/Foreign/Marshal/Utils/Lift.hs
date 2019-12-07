module Foreign.Marshal.Utils.Lift
( with
) where

import Control.Carrier.Lift
import Foreign.Ptr
import Foreign.Storable
import qualified Foreign.Marshal.Utils as U

with :: (Has (Lift IO) sig m, Storable a) => a -> (Ptr a -> m b) -> m b
with a action = liftWith $ \ ctx hdl -> U.with a (hdl . (<$ ctx) . action)
