{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Radar
( drawRadar
, runRadar
, Drawable
) where

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lens ((.=))
import           Control.Effect.Lift
import           Control.Effect.Profile
import           Control.Effect.State
import           Control.Monad (when)
import           Data.Coerce (coerce)
import           Data.Foldable (for_)
import           Data.Function ((&))
import           Data.Functor.Identity
import           Data.Functor.Interval
import           GL.Array
import           GL.Program
import           Lens.Micro (forOf_, traversed, (.~))
import           Linear.Exts as Linear
import           Starlight.Actor
import           Starlight.Body as Body hiding (Drawable)
import           Starlight.Character
import           Starlight.Radar.Shader
import           Starlight.System
import           Starlight.View
import           UI.Colour
import qualified UI.Drawable as UI
import           Unit.Angle
import           Unit.Length

drawRadar
  :: ( Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader Drawable) sig m
     , Has (Reader (System StateVectors)) sig m
     , Has (Reader View) sig m
     )
  => Character
  -> m ()
drawRadar Character{ actor = Actor{ position = here }, target } = measure "radar" . UI.using getDrawable $ do
  system@System{ scale, characters = npcs } <- ask @(System StateVectors)
  vs <- ask

  let radius = 100
  matrix_ .= Just (scaleToView vs)
  radius_ .= Just radius

  -- FIXME: skip blips for extremely distant objects
  -- FIXME: blips should shadow more distant blips
  measure "bodies" $
    forOf_ (bodies_ . traversed) system $ \ StateVectors{ body = Body{ radius = Metres r, colour }, position = there } -> do
      setBlip (makeBlip (there ^-^ here) (r * scale) colour)
      drawArrays LineStrip range

  measure "npcs" $ do
    sweep_  .= Just 0
    -- FIXME: fade colour with distance
    -- FIXME: IFF
    colour_ .= Just white

    forOf_ (traversed . actor_) npcs $ \ Actor{ position = there } -> let (angle, r) = polar2 (unP (there ^-^ here)) in when (r > zoom vs * radius) $ do
      angle_ .= Just angle
      drawArrays Points medianRange

  measure "targets" $ do
    let targetVectors = target >>= fmap (toBlip here scale) . (system !?)
    for_ targetVectors $ \ blip@Blip{ colour } -> do
      setBlip blip
      for_ [1..n] $ \ i -> do
        let radius = step * fromIntegral i
            -- FIXME: apply easing so this works more like a spring
            step = max 1 (min 50 (d blip / fromIntegral n))

        radius_ .= Just radius
        colour_ .= Just ((colour + 0.5 * fromIntegral i / fromIntegral n) ** 2 & _a .~ fromIntegral i / fromIntegral n)

        drawArrays LineStrip range
  where
  n = 10 :: Int

runRadar :: (Has Finally sig m, Has (Lift IO) sig m) => ReaderC Drawable m a -> m a
runRadar m = do
  program <- build shader
  array   <- load vertices
  runReader (Drawable UI.Drawable{ program, array }) m


toBlip :: Point V2 Float -> Float -> Either StateVectors Character -> Blip
toBlip here scale = either fromL fromR where
  fromL StateVectors{ body = Body{ radius = Metres r, colour }, position = there } = makeBlip (there ^-^ here) (r * scale) colour
  fromR Character{ actor = Actor{ position = there } } = makeBlip (there ^-^ here) 15 white

setBlip
  :: ( Has (Lift IO) sig m
     , Has (State (U Maybe)) sig m
     )
  => Blip
  -> m ()
setBlip Blip{ angle, direction, d, r, colour } = do
  angle_  .= Just angle
  sweep_  .= Just sweep
  colour_ .= Just colour
  where
  edge = perp direction ^* r + direction ^* d
  sweep = max minSweep (abs (wrap (Interval (-pi) pi) (angleOf edge - angle)))
  minSweep = 0.0133 -- at radius'=150, makes approx. 4px blips

data Blip = Blip
  { angle     :: Radians Float -- ^ angle to the object
  , d         :: Float         -- ^ distance to the object
  , direction :: V2 Float      -- ^ unit vector in the direction of the object
  , r         :: Float         -- ^ magnitude of the object
  , colour    :: Colour Float  -- ^ colour of the object
  }

makeBlip :: Point V2 Float -> Float -> Colour Float -> Blip
makeBlip (P there) r colour = Blip { angle, d, direction, r, colour } where
  angle = angleOf there
  d = norm there
  direction = normalize there


newtype Drawable = Drawable { getDrawable :: UI.Drawable U V O }


vertices :: [V Identity]
vertices = coerce @[Float] [ fromIntegral t / fromIntegral n | t <- [-n..n] ] where
  n = 16 :: Int

range :: Interval Identity Int
range = Interval 0 (Identity (length vertices))

medianRange :: Interval Identity Int
medianRange = Interval n (n + 1) where
  n = max_ range `div` 2
