module Foreign.C.String.Lift
( withCString
) where

import           Control.Carrier.Lift
import qualified Foreign.C.String as C

withCString :: Has (Lift IO) sig m => String -> (C.CString -> m a) -> m a
withCString s with = liftWith $ \ hdl ctx -> C.withCString s (hdl . (<$ ctx) . with)
