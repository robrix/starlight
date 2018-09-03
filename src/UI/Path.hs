{-# LANGUAGE DeriveFunctor, TypeApplications #-}
module UI.Path where

import Geometry.Triangle
import Linear.V2

data Path v n
  = M (v n) (Path v n)
  | L (v n) (Path v n)
  | Q (v n) (v n) (Path v n)
  | Z
  deriving (Eq, Functor, Show)


pathTriangles :: Int -> V2 a -> V2 a -> Path V2 a -> [(Triangle a, Kind)]
pathTriangles count first current p = case p of
  M v rest ->                                                                              pathTriangles 0            v     v  rest
  L v rest
    | count >= 1 -> (Triangle first current v,  Solid)                                   : pathTriangles (succ count) first v  rest
    | otherwise  ->                                                                        pathTriangles (succ count) first v  rest
  Q v1 v2 rest
    | count >= 1 -> (Triangle first current v2, Solid) : (Triangle current v1 v2, Curve) : pathTriangles (succ count) first v2 rest
    | otherwise  ->                                      (Triangle current v1 v2, Curve) : pathTriangles (succ count) first v2 rest
  Z ->                                                                                    []
