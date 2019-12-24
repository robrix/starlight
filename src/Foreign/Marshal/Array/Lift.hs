module Foreign.Marshal.Array.Lift
( allocaArray
, peekArray
, pokeArray
, withArray
, withArrayLen
) where

import           Control.Carrier.Lift
import qualified Foreign.Marshal.Array as A
import           Foreign.Ptr
import           Foreign.Storable

allocaArray :: (Has (Lift IO) sig m, Storable a) => Int -> (Ptr a -> m b) -> m b
allocaArray n with = liftWith $ \ ctx hdl -> A.allocaArray n (hdl . (<$ ctx) . with)

peekArray :: (Has (Lift IO) sig m, Storable a) => Int -> Ptr a -> m [a]
peekArray n = sendM . A.peekArray n

pokeArray :: (Has (Lift IO) sig m, Storable a) => Ptr a -> [a] -> m ()
pokeArray p = sendM . A.pokeArray p

withArray :: (Has (Lift IO) sig m, Storable a) => [a] -> (Ptr a -> m b) -> m b
withArray as with = liftWith $ \ ctx hdl -> A.withArray as (hdl . (<$ ctx) . with)

withArrayLen :: (Has (Lift IO) sig m, Storable a) => [a] -> (Int -> Ptr a -> m b) -> m b
withArrayLen as with = liftWith $ \ ctx hdl -> A.withArrayLen as (\ n -> hdl . (<$ ctx) . with n)
