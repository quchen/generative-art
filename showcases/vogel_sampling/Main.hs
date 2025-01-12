module Main (main) where



import Data.Colour.Names
import qualified Data.Vector as V
import Graphics.Rendering.Cairo as C

import Draw
import Geometry as G
import Geometry.Algorithms.Delaunay
import Geometry.Algorithms.Sampling.Vogel



picWidth, picHeight, squareSize :: (Num a, Ord a) => a
picWidth = 1440
picHeight = 1440
squareSize = min picWidth picHeight

file :: FilePath
file = "out/vogel_sampling.png"

main :: IO ()
main = do
    let center = Vec2 (picWidth/2) (picHeight/2)
        extents = boundingBox [Vec2 0 0, Vec2 picWidth picHeight]

    let seeds = filter (`insideBoundingBox` extents) $ vogel VogelSamplingParams
            { _vogelRadius = squareSize / sqrt 2
            , _vogelCenter = center
            , _vogelDensity = 1282.10688 / squareSize^2
            }
        cutoff = 0.47 * squareSize
    
    print (length seeds)

    let delaunay = delaunayTriangulation seeds
        cells = V.toList $ clipCellsToBox extents $ voronoiCells delaunay
        triangles = filter
            (\(Polygon ps) -> all (\p -> norm (p -. center) < cutoff) ps)
            (V.toList $ delaunayTriangles delaunay)
        voronoi = filter
            (\(seed, _) -> norm (seed -. center) < cutoff)
            (zip seeds cells)
        addCellGutter (seed, poly) =
            let cellGutter = 2 + norm (center -. seed) / (12*12)
            in  (seed, shrinkPolygon cellGutter poly)
        voronoiSmoothed = fmap (snd . fmap (chaikin 0.25 . chaikin 0.25 . chaikin 0.15) . addCellGutter) voronoi

    render file picWidth picHeight $ do
        cairoScope (setColor white >> paint)
        C.setLineJoin C.LineJoinBevel
        cairoScope $ do 
            setColor black
            setLineWidth 0.25
            for_ triangles $ \poly -> do
                sketch poly
                C.stroke
        cairoScope $ do
            setColor black
            setLineWidth 1
            for_ voronoiSmoothed $ \cell -> do
                sketch cell
                C.stroke

chaikin :: Double -> Polygon -> Polygon
chaikin _ (Polygon []) = Polygon []
chaikin lambda (Polygon ps@(p:_)) = Polygon $ concat
    [ [c, b]
    | (a, d) <- zip ps (tail ps ++ [p])
    , let b = lambda *. a +. (1-lambda) *. d
    , let c = (1-lambda) *. a +. lambda *. d
    ]
