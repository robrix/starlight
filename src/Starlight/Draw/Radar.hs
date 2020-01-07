{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Starlight.Draw.Radar
( drawRadar
, runRadar
, Drawable
, V(..)
) where

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lens (view, (?=))
import           Control.Effect.Lift
import           Control.Effect.Profile
import           Control.Effect.Trace
import           Control.Lens (Lens', (^.))
import           Data.Foldable (for_, toList)
import           Data.Functor.Identity
import           Data.Functor.Interval
import           Data.Generics.Product.Fields
import           Data.List (elemIndex)
import           Foreign.Storable (Storable(..))
import           GHC.Generics (Generic)
import           GL.Array
import           GL.Buffer as B
import           GL.Effect.Check
import           GL.Shader.DSL hiding (coerce, norm, (!*!), (^*), (^.), _a, _xy)
import qualified GL.Shader.DSL as D
import           Linear.Exts as Linear hiding (angle, (!*))
import           Starlight.Actor
import qualified Starlight.Body as B
import           Starlight.Character
import qualified Starlight.Ship as S
import           Starlight.System
import           Starlight.View
import qualified UI.Drawable as UI
import           Unit.Length

drawRadar
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader Drawable) sig m
     , Has (Reader (System B.StateVectors)) sig m
     , Has (Reader View) sig m
     )
  => m ()
drawRadar = measure "radar" . UI.using getDrawable $ do
  system@System{ scale, bodies } <- ask @(System B.StateVectors)
  target <- view (player_ @B.StateVectors .target_)
  here   <- view (player_ @B.StateVectors .actor_.position_)
  npcs <- view (npcs_ @B.StateVectors)
  vs <- ask

  let shipVertices = verticesForShips npcs
      bodyVertices = verticesForBodies scale bodies
      vertices = shipVertices <> bodyVertices

  measure "realloc/copy" $ do
    b <- askBuffer
    B.realloc b (length vertices) B.Static B.Draw
    B.copy b 0 vertices

  let radius = 100
      matrix = scaleToView vs
  matrix_ ?= matrix !*! scaled (V3 radius radius 1)
  here_   ?= here^._xy

  -- FIXME: skip blips for extremely distant objects
  -- FIXME: blips should shadow more distant blips
  -- FIXME: store the position, radius, & colour of each body at t and compute the blips in the (instanced?) shader rather than setting uniforms
  measure "bodies" $
    drawArrays Points (Interval (Identity (length shipVertices)) (Identity (length shipVertices + length bodyVertices)))

  measure "npcs" $ do
    -- FIXME: fade colour with distance
    -- FIXME: IFF
    drawArrays Points (Interval 0 (Identity (length shipVertices)))

  measure "targets" $ do
    for_ (target >>= (`elemIndex` identifiers system)) $ \ index ->
      for_ [1..n] $ \ _ -> do
        matrix_ ?= matrix !*! scaled (V3 radius radius 1)

        drawArrays Points (Interval (Identity index) (Identity index + 1))
  where
  n = 10 :: Int

runRadar :: (Effect sig, Has Check sig m, Has Finally sig m, Has (Lift IO) sig m, Has Trace sig m) => ReaderC Drawable m a -> m a
runRadar = UI.loadingDrawable Drawable shader []


newtype Drawable = Drawable { getDrawable :: UI.Drawable U V O }


verticesForBodies :: Foldable t => Float -> t B.StateVectors -> [V Identity]
verticesForBodies scale vs =
  [ vertex
  | B.StateVectors{ body = B.Body{ radius = Metres r, colour }, actor = Actor{ position = there } } <- toList vs
  , let vertex = V{ there = Identity (there^._xy), r = Identity (r * scale), colour = Identity colour }
  ]

verticesForShips :: Foldable t => t Character -> [V Identity]
verticesForShips cs =
  [ vertex
  | Character{ actor = Actor{ position = there }, ship = S.Ship { colour, scale } } <- toList cs
  , let vertex = V{ there = Identity (there^._xy), r = Identity scale, colour = Identity colour }
  ]


instanceCount :: Int
instanceCount = 1


shader :: Shader U V O
shader = program $ \ u
  ->  vertex (\ V{ there, r, colour } IF{ colour2 = out } -> main $ do
    there <- let' "there" (there - here u)
    d     <- let' "d"     (D.norm there)
    dir   <- let' "dir"   (there D.^* (1/D.norm there))
    let perp v = vec2 (negate (v D.^.D._y)) (v D.^.D._x)
        angleOf vec = atan2' (vec D.^.D._y) (vec D.^.D._x)
        wrap mn mx x = ((x + mx) `mod'` (mx - mn)) + mn
    edge  <- let' "edge"  (perp dir D.^* r + dir D.^* d)
    angle <- let' "angle" (angleOf there)
    sweep <- let' "sweep" (minSweep `D.max'` (abs (wrap (-pi) pi (angleOf edge - angle))))
    theta <- let' "theta" (angle + (float gl_InstanceID / fromIntegral instanceCount) * sweep)
    pos   <- let' "pos"   (vec2 (cos theta) (sin theta))
    gl_PointSize .= 3
    out .= colour
    gl_Position .= ext4 (ext3 ((matrix u !* ext3 pos 1) D.^. D._xy) 0) 1)

  >>> fragment (\ IF{ colour2 } O{ fragColour } -> main $ fragColour .= colour2) where
  minSweep = 0.0133 -- at radius'=150, makes approx. 4px blips


data U v = U
  { matrix :: v (M33 Float)
  , here   :: v (V2 Float)
  }
  deriving (Generic)

instance Vars U

matrix_ :: Lens' (U v) (v (M33 Float))
matrix_ = field @"matrix"

here_ :: Lens' (U v) (v (V2 Float))
here_ = field @"here"


newtype IF v = IF { colour2 :: v (Colour Float) }
  deriving (Generic)

instance Vars IF


data V v = V
  { there  :: v (V2 Float) -- location of object
  , r      :: v Float      -- radius of object
  , colour :: v (Colour Float)
  }
  deriving (Generic)

instance Vars V
deriving via Fields V instance Storable (V Identity)


newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars O
