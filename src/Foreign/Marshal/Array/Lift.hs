module Foreign.Marshal.Array.Lift
( allocaArray
, peekArray
, pokeArray
) where

import Control.Carrier.Lift
import Control.Monad.IO.Class
import qualified Foreign.Marshal.Array as A
import Foreign.Ptr
import Foreign.Storable

allocaArray :: (Has (Lift IO) sig m, Storable a) => Int -> (Ptr a -> m b) -> m b
allocaArray n with = liftWith $ \ ctx hdl -> A.allocaArray n (hdl . (<$ ctx) . with)

peekArray :: (MonadIO m, Storable a) => Int -> Ptr a -> m [a]
peekArray n = liftIO . A.peekArray n

pokeArray :: (MonadIO m, Storable a) => Ptr a -> [a] -> m ()
pokeArray p = liftIO . A.pokeArray p
