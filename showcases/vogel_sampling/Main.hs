{-# LANGUAGE RecordWildCards #-}
module Main (main) where



import Data.Colour.Names
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
import Control.Monad (replicateM, when)
import Control.Applicative (Applicative(liftA2))



picWidth, picHeight :: Num a => a
picWidth = 2560
picHeight = 1440

file :: FilePath
file = "out/voronoi_3d.png"

main :: IO ()
main = do
    let center = Vec2 720 720
    let points = filter (`insideBoundingBox` extents) $ vogel VogelSamplingParams
            { _vogelRadius = 720 * sqrt 2
            , _vogelCenter = center
            , _vogelDensity = 0.0005
            }
        cutoff = 680

    let seeds = V.toList $ V.last $ V.iterateN 5 (lloydRelaxation extents 1) $ V.fromList points
        delaunay = delaunayTriangulation seeds
        cells = V.toList $ clipCellsToBox extents $ voronoiCells delaunay
        voronoi = filter (\(seed, _) -> norm (seed -. center) < cutoff) $ zip seeds cells

    render file 1440 1440 $ do
        cairoScope (setColor white >> paint)
        for_ (delaunayTriangles delaunay) $ \poly@(Polygon ps) -> cairoScope $ do
            when (all (\p -> norm (p -. center) < cutoff) ps) $ do
                setColor mediumseagreen
                sketch poly
                C.stroke
        for_ voronoi $ \(seed, cell) -> do
            let cellGutter = 6 - norm (center -. seed) / (18*12)
            let l = 1 - norm (center -. seed) / cutoff
            setColor (black `withOpacity` l)
            drawCell (growPolygon (-cellGutter) cell)
  where
    extents = BoundingBox (Vec2 0 0) (Vec2 1440 1440)

drawCell :: Polygon -> Render ()
drawCell cell = cairoScope $ do
    C.setLineJoin C.LineJoinBevel
    sketch (chaikin 0.25 (chaikin 0.25 (chaikin 0.15 cell)))
    stroke

chaikin :: Double -> Polygon -> Polygon
chaikin _ (Polygon []) = Polygon []
chaikin lambda (Polygon ps@(p:_)) = Polygon $ concat
    [ [c, b]
    | (a, d) <- zip ps (tail ps ++ [p])
    , let b = lambda *. a +. (1-lambda) *. d
    , let c = (1-lambda) *. a +. lambda *. d
    ]
