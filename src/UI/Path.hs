{-# LANGUAGE DeriveFunctor, TypeApplications #-}
module UI.Path where

import Data.Bytes
import Data.Word
import Linear.V2

data Path v n
  = M (v n) (Path v n)
  | L (v n) (Path v n)
  | Q (v n) (v n) (Path v n)
  | Z
  deriving (Eq, Functor, Show)


encodePath :: Integral a => Path V2 a -> [Word8]
encodePath = go . fmap (toBytes @Word16 . fromIntegral)
  where go path = case path of
          M (V2 x y)            rest -> moveTo  : x ++ y             ++ go rest
          L (V2 x y)            rest -> lineTo  : x ++ y             ++ go rest
          Q (V2 x y) (V2 x' y') rest -> curveTo : x ++ y ++ x' ++ y' ++ go rest
          _                          -> close   : []
        (moveTo, lineTo, curveTo, close) = (0, 1, 2, 3)
