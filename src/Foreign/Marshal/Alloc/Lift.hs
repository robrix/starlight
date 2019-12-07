module Foreign.Marshal.Alloc.Lift
( alloca
, allocaBytes
) where

import Control.Carrier.Lift
import qualified Foreign.Marshal.Alloc as A
import Foreign.Ptr
import Foreign.Storable

alloca :: (Has (Lift IO) sig m, Storable a) => (Ptr a -> m b) -> m b
alloca with = liftWith $ \ ctx hdl -> A.alloca (hdl . (<$ ctx) . with)

allocaBytes :: Has (Lift IO) sig m => Int -> (Ptr a -> m b) -> m b
allocaBytes n with = liftWith $ \ ctx hdl -> A.allocaBytes n (hdl . (<$ ctx) . with)
