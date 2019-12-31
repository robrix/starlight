{-# LANGUAGE NamedFieldPuns #-}
module Starlight.System
( System(..)
, systemTrans
, _scale
, systemAt
) where

import           Data.Foldable (find)
import qualified Data.Map as Map
import           Lens.Micro (Lens', lens, (^.))
import           Linear.Epsilon
import           Linear.Matrix
import           Linear.V4
import           Linear.Vector
import           Starlight.Body
import           Starlight.Identifier
import           Unit.Time

data System f a = System
  { scale  :: !a
  , bodies :: !(Map.Map Identifier (f a))
  }
  deriving (Read, Show)

systemTrans :: Num a => System f a -> M44 a
systemTrans (System scale _) = scaled (V4 scale scale scale 1)

_scale :: Lens' (System f a) a
_scale = lens (\ System{ scale } -> scale) (\ System { bodies } s' -> System { bodies, scale = s' })



systemAt :: (Epsilon a, RealFloat a) => System Body a -> Seconds a -> System StateVectors a
systemAt sys@(System scale bs) t = System scale bs' where
  bs' = fmap go bs
  go b = StateVectors
    { body = b
    , transform = transform'
    , rotation = orientationAt b t
    , position = (transform' !* V4 0 0 0 1) ^. _xy
    } where
    rel = maybe (systemTrans sys) transform $ do
      p <- parent (identifier b)
      find ((== p) . identifier . body) bs'
    transform' = rel !*! transformAt (orbit b) t
