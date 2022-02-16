{-# LANGUAGE RecordWildCards #-}
module Main (main) where



import Data.List ( sortOn )
import Data.Maybe ( fromMaybe )
import Data.Ord ( comparing )
import qualified Data.Vector as V
import Graphics.Rendering.Cairo as C
import Math.Noise (Perlin (..), getValue, perlin)
import System.Random.MWC ( initialize, uniformRM )

import Draw
import Geometry as G
import Geometry.Algorithms.Delaunay
import Geometry.Algorithms.Sampling
import Geometry.Algorithms.Sampling.Vogel
import Control.Monad (replicateM)
import Control.Applicative (Applicative(liftA2))



picWidth, picHeight :: Num a => a
picWidth = 2560
picHeight = 1440

file :: FilePath
file = "out/voronoi_3d.png"

main :: IO ()
main = do
    let points = filter (`insideBoundingBox` extents) $ vogel VogelSamplingParams
            { _vogelRadius = 720 * sqrt(2)
            , _vogelCenter = Vec2 720 720
            , _vogelDensity = 0.000318
            }

    let cells = V.toList $ clipCellsToBox extents $ voronoiCells $ delaunayTriangulation points

    render file 1440 1440 $ do
        cairoScope (setColor white >> paint)
        for_ (zip points cells) $ \(seed, cell) -> do
            let cellGutter = 3 + norm (Vec2 720 720 -. seed) / 144
            drawCell (growPolygon (-cellGutter) cell)
  where
    extents = BoundingBox (Vec2 0 0) (Vec2 1440 1440)

drawCell :: Polygon -> Render ()
drawCell cell = cairoScope $ do
    C.setLineJoin C.LineJoinBevel
    sketch (chaikin 0.25 (chaikin 0.25 (chaikin 0.1 cell)))
    setColor black
    fill

chaikin :: Double -> Polygon -> Polygon
chaikin _ (Polygon []) = Polygon []
chaikin lambda (Polygon ps@(p:_)) = Polygon $ concat
    [ [c, b]
    | (a, d) <- zip ps (tail ps ++ [p])
    , let b = lambda *. a +. (1-lambda) *. d
    , let c = (1-lambda) *. a +. lambda *. d
    ]
