{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
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
) where

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lens (view, (?=))
import           Control.Effect.Lift
import           Control.Effect.Profile
import           Control.Effect.State
import           Control.Lens (Lens', forOf_, to, traversed, (.~), (^.))
import           Control.Monad (when)
import           Data.Coerce (coerce)
import           Data.Foldable (for_)
import           Data.Function ((&))
import           Data.Functor.Identity
import           Data.Functor.Interval
import           Data.Generics.Product.Fields
import           Foreign.Storable (Storable)
import           GHC.Generics (Generic)
import           GL.Array
import           GL.Effect.Check
import           GL.Object
import           GL.Shader.DSL hiding (coerce, norm, (^*), (^.), _a, _xy)
import qualified GL.Shader.DSL as D
import           Linear.Exts as Linear hiding (angle, (!*))
import           Starlight.Actor
import qualified Starlight.Body as B
import           Starlight.Character
import qualified Starlight.Ship as S
import           Starlight.System
import           Starlight.View
import           UI.Colour
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

  let radius = 100
  matrix_ ?= scaleToView vs
  radius_ ?= radius

  -- FIXME: skip blips for extremely distant objects
  -- FIXME: blips should shadow more distant blips
  -- FIXME: store the position, radius, & colour of each body at t and compute the blips in the (instanced?) shader rather than setting uniforms
  measure "bodies" $
    for_ bodies $ \ B.StateVectors{ body = B.Body{ radius = Metres r, colour }, actor = Actor{ position = there } } -> do
      measure "setBlip" $ setBlip (makeBlip (there ^-^ here) (r * scale) colour)
      measure "drawArrays" $ drawArrays LineStrip range

  measure "npcs" $ do
    sweep_  ?= 0
    -- FIXME: fade colour with distance
    -- FIXME: IFF
    forOf_ traversed npcs $ \ Character{ actor = Actor{ position = there }, ship = S.Ship { colour, armour } } -> do
      let (angle, r) = polar2 (unP (there ^-^ here) ^. _xy)
      when (r > zoom vs * radius && armour^.min_ > 0) $ do
        colour_ ?= (colour & _a .~ armour^.min_.to runIdentity / armour^.max_.to runIdentity)
        angle_  ?= angle
        drawArrays Points medianRange

  measure "targets" $ do
    let targetVectors = target >>= fmap (toBlip here scale) . (system !?)
    for_ targetVectors $ \ blip@Blip{ colour' } -> do
      setBlip blip
      for_ [1..n] $ \ i -> do
        let radius = step * fromIntegral i
            -- FIXME: apply easing so this works more like a spring
            step = max 1 (min 50 ((1/zoom vs) * d blip / fromIntegral n))

        radius_ ?= radius
        colour_ ?= ((colour' + 0.5 * fromIntegral i / fromIntegral n) ** 2 & _a .~ fromIntegral i / fromIntegral n)

        drawArrays LineStrip range
  where
  n = 10 :: Int

runRadar :: (Effect sig, Has Check sig m, Has Finally sig m, Has (Lift IO) sig m) => ReaderC Drawable m a -> m a
runRadar = UI.loadingDrawable Drawable shader vertices


toBlip :: Point V3 Float -> Float -> Either B.StateVectors Character -> Blip
toBlip here scale = either fromL fromR where
  fromL B.StateVectors{ body = B.Body{ radius = Metres r, colour }, actor = Actor{ position = there } } = makeBlip (there ^-^ here) (r * scale) colour
  fromR Character{ actor = Actor{ position = there }, ship = S.Ship { colour, scale } } = makeBlip (there ^-^ here) scale colour

setBlip
  :: ( Has (Lift IO) sig m
     , Has (State (U Maybe)) sig m
     )
  => Blip
  -> m ()
setBlip Blip{ angle', direction, d, r, colour' } = do
  angle_  ?= angle'
  sweep_  ?= sweep
  colour_ ?= colour'
  where
  edge = perp direction ^* r + direction ^* d
  sweep = max minSweep (abs (wrap (Interval (-pi) pi) (angleOf edge - angle')))
  minSweep = 0.0133 -- at radius'=150, makes approx. 4px blips

data Blip = Blip
  { angle'    :: !(Radians Float) -- ^ angle to the object
  , d         :: !Float           -- ^ distance to the object
  , direction :: !(V2 Float)      -- ^ unit vector in the direction of the object
  , r         :: !Float           -- ^ magnitude of the object
  , colour'   :: !(Colour Float)  -- ^ colour of the object
  }

makeBlip :: Point V3 Float -> Float -> Colour Float -> Blip
makeBlip (P there) r colour' = Blip{ angle', d, direction, r, colour' } where
  there' = there ^. _xy
  angle' = angleOf there'
  d = norm there
  direction = normalize there'


newtype Drawable = Drawable { getDrawable :: UI.Drawable U V O }


vertices :: [V Identity]
vertices = coerce @[Float] [ fromIntegral t / fromIntegral n | t <- [-n..n] ] where
  n = 16 :: Int

range :: Interval Identity Int
range = Interval 0 (Identity (length vertices))

medianRange :: Interval Identity Int
medianRange = Interval n (n + 1) where
  n = range^.max_ `div` 2


shader :: Shader U V O
shader = program $ \ u
  ->  vertex (\ V{ n } None -> do
    angle <- let' "angle" (D.coerce (angle u) + n * D.coerce (sweep u))
    pos   <- let' "pos"   (vec2 (cos angle) (sin angle) D.^* radius u)
    gl_PointSize .= 3
    gl_Position .= ext4 (ext3 ((matrix u !* ext3 pos 1) D.^. D._xy) 0) 1)

  >>> fragment (\ None O{ fragColour } -> fragColour .= colour u)


data U v = U
  { matrix :: v (M33 Float)
  , radius :: v Float
  , angle  :: v (Radians Float)
  , sweep  :: v (Radians Float)
  , colour :: v (Colour Float)
  }
  deriving (Generic)

instance Vars U

matrix_ :: Lens' (U v) (v (M33 Float))
matrix_ = field @"matrix"

radius_ :: Lens' (U v) (v Float)
radius_ = field @"radius"

angle_ :: Lens' (U v) (v (Radians Float))
angle_ = field @"angle"

sweep_ :: Lens' (U v) (v (Radians Float))
sweep_ = field @"sweep"

colour_ :: Lens' (U v) (v (Colour Float))
colour_ = field @"colour"


newtype V v = V { n :: v Float }
  deriving (Generic)

instance Vars V

deriving instance Bind     (v Float) => Bind     (V v)
deriving instance Storable (v Float) => Storable (V v)

newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars O
