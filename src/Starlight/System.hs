{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Starlight.System
( System(..)
, systemTrans
, scale_
, bodies_
, characters_
, identifiers
, (!?)
) where

import           Control.Lens (Lens, Lens', lens)
import qualified Data.Map as Map
import           Linear.Matrix
import           Linear.V4
import           Linear.Vector
import           Starlight.Character
import           Starlight.Identifier

data System a = System
  { scale      :: !Float
  , bodies     :: !(Map.Map BodyIdentifier a)
  , characters :: ![Character]
  }
  deriving (Show)

systemTrans :: System a -> M44 Float
systemTrans System{ scale } = scaled (V4 scale scale scale 1)

scale_ :: Lens' (System a) Float
scale_ = lens scale (\ s scale -> s { scale })

bodies_ :: Lens (System a) (System b) (Map.Map BodyIdentifier a) (Map.Map BodyIdentifier b)
bodies_ = lens bodies (\ s bodies -> s { bodies })

characters_ :: Lens' (System a) [Character]
characters_ = lens characters (\ s characters -> s { characters })

identifiers :: System a -> [Identifier]
identifiers System{ bodies, characters } = map S [0..pred (length characters)] <> map B (Map.keys bodies)

(!?) :: System a -> Identifier -> Maybe (Either a Character)
(!?) System{ bodies, characters } = \case
  B i -> Left  <$> Map.lookup i bodies
  S i -> Right <$> characters !? i where
    []     !? _  = Nothing
    (x:xs) !? i
      | i == 0    = Just x
      | otherwise = xs !? pred i
