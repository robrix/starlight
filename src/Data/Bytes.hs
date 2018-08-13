module Data.Bytes where

import Data.Word

class Bytes t where
  toBytes :: t -> [Word8]
