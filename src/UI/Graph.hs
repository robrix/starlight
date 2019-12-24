{-# LANGUAGE DataKinds, DisambiguateRecordFields, NamedFieldPuns, TypeApplications, TypeOperators #-}
module UI.Graph
( Graph
, mkGraph
, drawGraph
) where

import Control.Carrier.Finally
import Control.Monad.IO.Class.Lift
import Data.Interval
import GL.Array
import GL.Effect.Program
import GL.Object
import qualified GL.Program as GL
import Graphics.GL.Core41
import Lens.Micro ((^.))
import Linear.Exts
import Linear.Matrix
import Linear.V2
import Linear.V4
import Linear.Vector
import UI.Colour
import qualified UI.Graph.Lines as Lines
import qualified UI.Graph.Points as Points

data Graph = Graph
  { matrix    :: !(M33 Float)
  , colour    :: !(V4 Float)
  , array     :: !(Array (V2 Float))
  , points    :: !(GL.Program Points.U)
  , lines     :: !(GL.Program Lines.U)
  , pointSize :: !Float
  , count     :: !Int
  }

mkGraph :: (Has Finally sig m, Has (Lift IO) sig m, Has Program sig m) => (Float -> Float) -> Int -> Float -> Float -> m Graph
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
  array <- loadVertices vertices
  points <- build' Points.shader
  lines <- build' Lines.shader

  pure $! Graph { matrix, colour, array, points, lines, pointSize = 9, count }

drawGraph :: (Has (Lift IO) sig m, Has Program sig m) => Graph -> m ()
drawGraph Graph { matrix, colour, array, points, lines, pointSize, count } = do
  bind (Just array)
  runLiftIO (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  use points $ do
    set Points.U { matrix = Just matrix, pointSize = Just pointSize, colour = Just colour }
    drawArrays Points    (Interval 0 count)
  use lines $ do
    set Lines.U { matrix = Just matrix, colour = Just colour }
    drawArrays LineStrip (Interval 0 count)
