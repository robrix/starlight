{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
import           Data.Functor.I
import           Data.Functor.Interval
import           Data.Generics.Product.Fields
import           Data.List (elemIndex)
import           Foreign.Storable (Storable(..))
import           GHC.Generics (Generic)
import           GL.Array
import           GL.Buffer as B
import           GL.Effect.Check
import           GL.Shader.DSL hiding (coerce, norm, (!*!), (^*), (^.), _a, _xy, _xyz)
import qualified GL.Shader.DSL as D
import           Linear.Exts as Linear hiding ((!*))
import           Starlight.Actor
import qualified Starlight.Body as B
import           Starlight.Character
import qualified Starlight.Ship as S
import           Starlight.System
import           Starlight.View
import qualified UI.Drawable as UI
import qualified UI.Window as Window
import           Unit.Angle
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
drawRadar = UI.using getDrawable $ do
  view@View{ scale } <- ask
  system@System{ bodies } <- ask @(System B.StateVectors)
  let target   = system^.player_.target_
      here     = system^.player_.actor_.position_._xy
      npcs     = system^.npcs_
      vertices = verticesForShips scale npcs <> verticesForBodies bodies

  measure "realloc/copy" $ do
    B.realloc (length vertices) B.Static B.Draw
    B.copy 0 vertices

  matrix_ ?= tmap realToFrac (transformToWindow view)
  here_   ?= (fmap realToFrac <$> here)

  -- FIXME: skip blips for extremely distant objects
  -- FIXME: blips should shadow more distant blips
  -- FIXME: fade colour with distance
  -- FIXME: IFF
  measure "bodies & npcs" $
    drawArrays Points (Interval 0 (I (length vertices)))

  measure "targets" $
    for_ (target >>= (`elemIndex` drop 1 (identifiers system))) $ \ index ->
      drawArraysInstanced Points (Interval (I index) (I index + 1)) targetBlipCount

runRadar :: (Effect sig, Has Check sig m, Has Finally sig m, Has (Lift IO) sig m, Has Trace sig m) => ReaderC Drawable m a -> m a
runRadar = UI.loadingDrawable Drawable shader []


newtype Drawable = Drawable { getDrawable :: UI.Drawable U V O }


verticesForBodies :: Foldable t => t B.StateVectors -> [V I]
verticesForBodies vs =
  [ V{ there = I (realToFrac <$> (there^._xy)), r = I (realToFrac <$> (b^.actor_.magnitude_)), colour = I colour }
  | b@B.StateVectors{ body = B.Body{ colour }, actor = Actor{ position = there } } <- toList vs
  ]

-- FIXME: take ship profile into account
verticesForShips :: Foldable t => Double -> t Character -> [V I]
verticesForShips scale cs =
  [ V{ there = I (realToFrac <$> (there^._xy)), r = I (realToFrac <$> (c^.actor_.magnitude_ ^/ scale)), colour = I colour }
  | c@Character{ actor = Actor{ position = there }, ship = S.Ship { colour } } <- toList cs
  ]


targetBlipCount :: Int
targetBlipCount = 10

shader :: Shader U V O
shader = program $ \ u
  ->  vertex (\ V{ there, r, colour } IG{ colour2, sweep } -> main $ do
    there <- let' "there" (D.coerce there - D.coerce (here u))
    d     <- let' "d"     (D.norm there)
    dir   <- let' "dir"   (there D.^* (1/d))
    let perp v = vec2 [negate (v D.^.D._y), v D.^.D._x]
        angleOf vec = atan2' (vec D.^.D._y) (vec D.^.D._x)
        wrap mn mx x = ((x + mx) `mod'` (mx - mn)) + mn
    edge  <- let' "edge"  (perp dir D.^* D.coerce r D.^* 0.5 + there)
    angle <- let' "angle" (D.coerce (angleOf there))
    radius <- var "radius" radius
    let step = D.max' 1 (D.min' maxStep (D.coerce d / (50 / 1000))) -- FIXME: account for unit size without hard-coding conversion factor
    iff (gl_InstanceID `gt` 0)
      (radius .= step * float gl_InstanceID)
      (pure ())
    minSweep <- let' "minSweep" (D.coerce (minBlipSize / (2 * pi * get radius)))
    sweep .= (minSweep `D.max'` abs (wrap (-pi) pi (D.coerce (angleOf edge) - angle)))
    pos   <- let' "pos"   (vec2 [cos angle, sin angle] D.^* D.coerce (get radius))
    colour2 .= colour
    gl_Position .= ext4 (ext3 pos 0) 1)

  >>> geometry (\ IG{} IF{ colour3 } -> do
    primitiveIn Points
    primitiveOut LineStrip (count * 2 + 1)
    main $ do
      let rot theta = mat4
            (vec4 [cos theta, -(sin theta), 0, 0])
            (vec4 [sin theta,  cos  theta , 0, 0])
            (vec4 [0,          0,           1, 0])
            (vec4 [0,          0,           0, 1])
      emitPrimitive $ do
        i <- var @Int "i" (-(fromIntegral count))
        while (get i `lt` (fromIntegral count + 1)) $
          emitVertex $ do
            theta <- let' "theta" (float (get i) / float (fromIntegral count) * Var "sweep[0]")
            gl_Position .= D.coerce (matrix u) D.!*! rot theta !* Var "gl_in[0].gl_Position"
            colour3 .= Var "colour2[0]"
            i += 1)

  >>> fragment (\ IF{ colour3 } O{ fragColour } -> main $ fragColour .= colour3) where
  minBlipSize = 16
  count = 16
  radius = 300
  maxStep = radius / fromIntegral targetBlipCount


data U v = U
  { matrix :: v (Transform Float ClipUnits Window.Pixels)
  , here   :: v (V2 (Mega Metres Float))
  }
  deriving (Generic)

instance Vars U

matrix_ :: Lens' (U v) (v (Transform Float ClipUnits Window.Pixels))
matrix_ = field @"matrix"

here_ :: Lens' (U v) (v (V2 (Mega Metres Float)))
here_ = field @"here"


data IG v = IG
  { colour2 :: v (Colour Float)
  , sweep   :: v (Radians Float)
  }
  deriving (Generic)

instance Vars IG

newtype IF v = IF { colour3 :: v (Colour Float) }
  deriving (Generic)

instance Vars IF


data V v = V
  { there  :: v (V2 (Mega Metres Float)) -- location of object
  , r      :: v (Mega Metres Float)      -- radius of object
  , colour :: v (Colour Float)
  }
  deriving (Generic)

instance Vars V
deriving via Fields V instance Storable (V I)


newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars O
