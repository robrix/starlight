module Data.Bytes where

import Data.Bits
import Data.Word

class Bytes t where
  toBytes :: t -> [Word8]

instance Bytes Word8 where
  toBytes = pure

instance Bytes Word16 where
  toBytes x = [ fromIntegral $ x .&. 0xFF, fromIntegral $ (x .&. 0xFF00) `shiftR` 8 ]
