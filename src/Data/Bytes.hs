module Data.Bytes where

import Data.Word

class Bytes t where
  toBytes :: t -> [Word8]

instance Bytes Word8 where
  toBytes = pure
