{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module UI.Graph
( Graph
, mkGraph
, drawGraph
) where

import           Control.Carrier.Finally
import           Control.Monad.IO.Class.Lift
import           Data.Coerce
import           Data.Functor.I
import           Data.Functor.Interval
import           GL.Array
import           GL.Program
import           Graphics.GL.Core41
import           Lens.Micro ((^.))
import           Linear.Exts
import           Linear.Matrix
import           Linear.V2
import           Linear.V4
import           Linear.Vector
import           UI.Colour
import qualified UI.Graph.Lines as Lines
import qualified UI.Graph.Points as Points
import           UI.Graph.Vertex

data Graph = Graph
  { matrix    :: !(M33 Float)
  , colour    :: !(V4 Float)
  , array     :: !(Array (V I))
  , points    :: !(Program Points.U V Points.O)
  , lines     :: !(Program Lines.U  V Lines.O)
  , pointSize :: !Float
  , count     :: !Int
  }

mkGraph :: (Has Finally sig m, Has (Lift IO) sig m) => (Float -> Float) -> Int -> Float -> Float -> m Graph
mkGraph f n from to = do
  let vertex = V2 <*> f
      count = max n 0 + 2
      vertices = map (\ i -> vertex (from + (to - from) * fromIntegral i / fromIntegral (count - 1))) [0..n+1]
      minXY = V2 from (minimum (map (^. _y) vertices))
      maxXY = V2 to   (maximum (map (^. _y) vertices))
      matrix
        =   translated (-1)
        !*! scaled     (ext (2 / (maxXY - minXY)) 1)
        !*! translated (negated minXY)
      colour = white
  array <- load (coerce @[V2 Float] vertices)
  points <- build Points.shader
  lines <- build Lines.shader

  pure $! Graph { matrix, colour, array, points, lines, pointSize = 9, count }

drawGraph :: (Has Finally sig m, Has (Lift IO) sig m) => Graph -> m ()
drawGraph Graph { matrix, colour, array, points, lines, pointSize, count } = bindArray array $ do
  runLiftIO (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  use points $ do
    set Points.U { matrix = Just matrix, pointSize = Just pointSize, colour = Just colour }
    drawArrays Points    (Interval 0 count)
  use lines $ do
    set Lines.U { matrix = Just matrix, colour = Just colour }
    drawArrays LineStrip (Interval 0 count)
