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
import           Control.Effect.Lens ((?=))
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
  let target   = system^.player_.target_
      here     = system^.player_.actor_.position_._xy
      npcs     = system^.npcs_
      vertices = verticesForShips npcs <> verticesForBodies scale bodies

  measure "realloc/copy" $ do
    B.realloc (length vertices) B.Static B.Draw
    B.copy 0 vertices

  matrix <- asks scaleToView
  matrix_ ?= matrix
  here_   ?= here

  -- FIXME: skip blips for extremely distant objects
  -- FIXME: blips should shadow more distant blips
  -- FIXME: fade colour with distance
  -- FIXME: IFF
  measure "bodies & npcs" $
    drawArrays Points (Interval 0 (Identity (length vertices)))

  measure "targets" $
    for_ (target >>= (`elemIndex` drop 1 (identifiers system))) $ \ index ->
      drawArraysInstanced Points (Interval (Identity index) (Identity index + 1)) targetBlipCount

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


targetBlipCount :: Int
targetBlipCount = 10

shader :: Shader U V O
shader = program $ \ u
  ->  vertex (\ V{ there, r, colour } IG{ colour2, sweep } -> main $ do
    there <- let' "there" (there - here u)
    d     <- let' "d"     (D.norm there)
    dir   <- let' "dir"   (there D.^* (1/D.norm there))
    let perp v = vec2 (negate (v D.^.D._y)) (v D.^.D._x)
        angleOf vec = atan2' (vec D.^.D._y) (vec D.^.D._x)
        wrap mn mx x = ((x + mx) `mod'` (mx - mn)) + mn
    edge  <- let' "edge"  (perp dir D.^* r + dir D.^* d)
    angle <- let' "angle" (angleOf there)
    radius <- var "radius" radius
    let step = D.max' 1 (D.min' (get radius/float (fromIntegral targetBlipCount)) (d / 50))
    iff (gl_InstanceID `gt` 0)
      (radius .= step * float gl_InstanceID)
      (pure ())
    minSweep <- let' "minSweep" (minBlipSize / (2 * pi * get radius))
    sweep .= (minSweep `D.max'` (abs (wrap (-pi) pi (angleOf edge - angle))))
    pos   <- let' "pos"   (vec2 (cos angle) (sin angle) D.^* get radius)
    gl_PointSize .= 3
    colour2 .= colour
    gl_Position .= ext4 (ext3 pos 0) 1)

  >>> geometry (\ IG{} IF{ colour3 } -> do
    primitiveIn Points
    primitiveOut LineStrip (count * 2 + 1)
    main $ do
      let rot theta = mat3
            (vec3 (cos theta) (-(sin theta)) 0)
            (vec3 (sin theta) (cos   theta)  0)
            (vec3 0           0              1)
      emitPrimitive $ do
        i <- var @Int "i" (-(fromIntegral count))
        while (get i `lt` (fromIntegral count + 1)) $
          emitVertex $ do
            theta <- let' "theta" (float (get i) / float (fromIntegral count) * Var "sweep[0]")
            gl_Position .= ext4 (matrix u D.!*! rot theta !* Var "gl_in[0].gl_Position.xyz") 1
            colour3 .= Var "colour2[0]"
            i += 1)

  >>> fragment (\ IF{ colour3 } O{ fragColour } -> main $ fragColour .= colour3) where
  minBlipSize = 16
  count = 16
  radius = 300


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


data IG v = IG
  { colour2 :: v (Colour Float)
  , sweep   :: v Float
  }
  deriving (Generic)

instance Vars IG

newtype IF v = IF { colour3 :: v (Colour Float) }
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
