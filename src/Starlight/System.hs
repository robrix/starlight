{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Starlight.System
( System(..)
, systemTrans
, scale_
, bodies_
, actors_
, identifiers
, (!?)
) where

import           Data.List
import qualified Data.Map as Map
import           Lens.Micro (Lens, Lens', lens)
import           Linear.Matrix
import           Linear.V4
import           Linear.Vector
import           Starlight.Actor
import           Starlight.Identifier

data System a = System
  { scale  :: !Float
  , bodies :: !(Map.Map BodyIdentifier a)
  , actors :: ![Actor]
  }
  deriving (Show)

systemTrans :: System a -> M44 Float
systemTrans System{ scale } = scaled (V4 scale scale scale 1)

scale_ :: Lens' (System a) Float
scale_ = lens scale (\ s scale -> s { scale })

bodies_ :: Lens (System a) (System b) (Map.Map BodyIdentifier a) (Map.Map BodyIdentifier b)
bodies_ = lens bodies (\ s bodies -> s { bodies })

actors_ :: Lens' (System a) [Actor]
actors_ = lens actors (\ s actors -> s { actors })

identifiers :: System a -> [Identifier]
identifiers = map B . Map.keys . bodies

(!?) :: System a -> Identifier -> Maybe (Either a Actor)
(!?) System{ bodies, actors } = \case
  B i -> Left        <$> Map.lookup i bodies
  S i -> Right . fst <$> uncons (drop (pred i) actors)
