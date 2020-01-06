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
import           Unit.Angle (Radians(..))
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
  Character{ actor = Actor{ position = here }, target } <- view (player_ @B.StateVectors)
  npcs <- view (npcs_ @B.StateVectors)
  vs <- ask

  let shipVertices = verticesForShips here npcs
      bodyVertices = verticesForBodies here scale bodies
      vertices = shipVertices <> bodyVertices

  measure "realloc/copy" $ do
    b <- askBuffer
    B.realloc b (length vertices) B.Static B.Draw
    B.copy b 0 vertices

  let radius = 100
      matrix = scaleToView vs
  matrix_ ?= matrix !*! scaled (V3 radius radius 1)

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


makeVertex :: Point V3 Float -> Float -> Colour Float -> V Identity
makeVertex (P there) r colour = V{ angle = Identity angle, sweep = Identity sweep, colour = Identity colour } where
  there' = there ^. _xy
  angle = angleOf there'
  d = norm there
  direction = normalize there'
  edge = perp direction ^* r + direction ^* d
  sweep = max minSweep (abs (wrap (Interval (-pi) pi) (angleOf edge - angle)))
  minSweep = 0.0133 -- at radius'=150, makes approx. 4px blips


verticesForBodies :: Foldable t => Point V3 Float -> Float -> t B.StateVectors -> [V Identity]
verticesForBodies here scale vs =
  [ vertex
  | B.StateVectors{ body = B.Body{ radius = Metres r, colour }, actor = Actor{ position = there } } <- toList vs
  , let vertex = makeVertex (there ^-^ here) (r * scale) colour
  ]

verticesForShips :: Foldable t => Point V3 Float -> t Character -> [V Identity]
verticesForShips here cs =
  [ vertex
  | Character{ actor = Actor{ position = there }, ship = S.Ship { colour, scale } } <- toList cs
  , let vertex = makeVertex (there ^-^ here) scale colour
  ]


shader :: Shader U V O
shader = program $ \ u
  ->  vertex (\ V{ angle, sweep, colour } IF{ colour2 = out } -> do
    angle <- let' "angle" (D.coerce angle + float gl_InstanceID * D.coerce sweep)
    pos   <- let' "pos"   (vec2 (cos angle) (sin angle))
    gl_PointSize .= 3
    out .= colour
    gl_Position .= ext4 (ext3 ((matrix u !* ext3 pos 1) D.^. D._xy) 0) 1)

  >>> fragment (\ IF{ colour2 } O{ fragColour } -> fragColour .= colour2)


newtype U v = U { matrix :: v (M33 Float) }
  deriving (Generic)

instance Vars U

matrix_ :: Lens' (U v) (v (M33 Float))
matrix_ = field @"matrix"


newtype IF v = IF { colour2 :: v (Colour Float) }
  deriving (Generic)

instance Vars IF


data V v = V
  { angle  :: v (Radians Float)
  , sweep  :: v (Radians Float)
  , colour :: v (Colour Float)
  }
  deriving (Generic)

instance Vars V
deriving via Fields V instance Storable (V Identity)


newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars O
