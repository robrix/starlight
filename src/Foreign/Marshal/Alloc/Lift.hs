module Foreign.Marshal.Alloc.Lift
( alloca
, allocaBytes
) where

import           Control.Carrier.Lift
import qualified Foreign.Marshal.Alloc as A
import           Foreign.Ptr
import           Foreign.Storable

alloca :: (Has (Lift IO) sig m, Storable a) => (Ptr a -> m b) -> m b
alloca with = liftWith $ \ hdl ctx -> A.alloca (hdl . (<$ ctx) . with)

allocaBytes :: Has (Lift IO) sig m => Int -> (Ptr a -> m b) -> m b
allocaBytes n with = liftWith $ \ hdl ctx -> A.allocaBytes n (hdl . (<$ ctx) . with)
