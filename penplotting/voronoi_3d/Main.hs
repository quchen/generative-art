{-# LANGUAGE LambdaCase #-}
module Main (main) where



import           Data.List
import           Data.Maybe
import           Data.Ord
import qualified Data.Vector              as V
import           Graphics.Rendering.Cairo as C hiding (height, width, x, y)
import           Math.Noise               (Perlin (..), getValue, perlin)
import           Prelude                  hiding ((**))
import           System.Random.MWC

import Draw
import Geometry                     as G
import Geometry.Algorithms.Delaunay
import Geometry.Algorithms.Sampling
import Control.Monad (guard)
import Debug.Trace (traceShowM, traceShow, traceShowId)
import System.Environment (getArgs)



picWidth, picHeight :: Num a => a
picWidth = 2560
picHeight = 1440

scaleFactor :: Double
scaleFactor = 1

epsilon :: Double
epsilon = 0.01

main :: IO ()
main = do
    seed <- read . head <$> getArgs

    let file = "out/voronoi_3d.png"
        count = 200
        scaledWidth = round (scaleFactor * picWidth)
        scaledHeight = round (scaleFactor * picHeight)

    gen <- initialize (V.fromList [seed])
    let -- constructed so that we have roughly `count` points
        adaptiveRadius = 1440 * sqrt (0.75 / count)
        poissonShape  = boundingBox [zero, Vec2 1440 1440]
        poissonRadius = adaptiveRadius
        poissonK      = 4
        bounds        = BoundingBox  (Vec2 0 0) (Vec2 1440 1440)

    points <- poissonDisc gen poissonShape poissonRadius poissonK
    print (length points)
    let voronoi
            = clipCellsToBox bounds
            $ voronoiCells
            $ delaunayTriangulation
            $ V.last
            $ V.iterateN 4 (lloydRelaxation bounds 1)
            $ V.fromList points
        voronoiWithProps = (\(seed, region) -> (seed, randomColor seed, randomHeight seed, region)) <$> zip points (V.toList voronoi)
        origin = Vec2 (picWidth/2) (picHeight/2)
        cells = sortOn (\(Polygon ps, _, _) -> minimum (_y <$> ps)) $ do
            (seed, color, height, region) <- voronoiWithProps
            let region' = G.transform
                    (  G.translate (Vec2 0 (-picHeight/5))
                    <> G.scaleAround' origin 1 0.35
                    <> G.rotateAround origin (deg 45)
                    <> G.translate (Vec2 560 0 )
                    <> G.scaleAround seed 0.9 )
                    region
            pure (region', color, height)

    render file scaledWidth scaledHeight $ do
        coordinateSystem (MathStandard_ZeroBottomLeft_XRight_YUp picHeight)
        cairoScope (setColor (magma 0.05) >> paint)
        drawCells cells

randomHeight :: Vec2 -> Double
randomHeight = \p -> 300 + 400 * noise2d p + 200 * exp(- 0.000005 * normSquare (p -. origin))
  where
    noise = perlin { perlinOctaves = 4, perlinFrequency = 0.001, perlinSeed = 1980166 }
    noise2d (Vec2 x y) = fromMaybe 0 $ getValue noise (x, y, 0)
    origin = Vec2 720 720

randomColor :: Vec2 -> Color Double
randomColor = \p -> inferno (0.6 + 0.35 * noise2d p)
  where
    noise = perlin { perlinOctaves = 5, perlinFrequency = 0.001, perlinPersistence = 0.65, perlinSeed = 1980166 }
    noise2d (Vec2 x y) = fromMaybe 0 $ getValue noise (x, y, 0)

drawCells :: [(Polygon, Color Double, Double)] -> Render ()
drawCells cells = do
    let bottomTopPolys = (\(p, _, h) -> (p, G.transform (G.translate (Vec2 0 h)) p)) <$> cells
        sidePolys = do
            (Polygon ps, _, height) <- cells
            (p, q) <- zip ps (drop 1 (cycle ps))
            let p1 = Polygon [p, q, q +. Vec2 0 height, p +. Vec2 0 height]
                p2 = Polygon [q, p, p +. Vec2 0 height, q +. Vec2 0 height]
            pure $ case polygonOrientation p1 of
                PolygonPositive -> p1
                PolygonNegative -> p2
        sideEdges = do
            (Polygon ps, c, h) <- cells
            p <- ps
            pure (c, Line p (p +. Vec2 0 h))
        bottomTopEdges = do
            (p, q) <- bottomTopPolys
            zip (polygonEdges p) (polygonEdges q)
    for_ sideEdges $ \(color, line@(Line p _)) -> cairoScope $ do
        -- growPolygon to avoid degenerate cases when intersecting polygons (i.e. clipping coinciding edges)
        let shadowingSidePolys = fmap (growPolygon epsilon) $ flip filter sidePolys $ \(Polygon [p', q', _, _])
                ->  _x p `between` (_x p', _x q')
                && cross (q' -. p') (p -. p') > 0
            shadowingTopPolys = fmap (growPolygon epsilon) $ do
                (bottomPoly, topPoly) <- bottomTopPolys
                let testRay = Line p (p -. Vec2 0 picHeight) -- very long test ray
                let intersections = do
                        edge <- polygonEdges (shrinkPolygon epsilon bottomPoly)
                        case intersectionLL testRay edge of
                            IntersectionReal i -> pure i
                            _ -> []
                case length intersections of
                    2 -> [topPoly]
                    _ -> []
        let (fg, bg1, bg2, bg3) = clipLine (shadowingSidePolys ++ shadowingTopPolys) line
        for_ fg $ \fgl -> cairoScope $ do
            setLineWidth 4
            setColor color
            sketch fgl
            stroke
        for_ bg1 $ \bgl -> cairoScope $ do
            setLineWidth 2
            setColor (color `withOpacity` 0.75)
            sketch bgl
            stroke
        for_ (bg2 ++ bg3) $ \bgl -> cairoScope $ do
            setLineWidth 1
            setColor (color `withOpacity` 0.5)
            sketch bgl
            stroke
    for_ bottomTopEdges $ \(bottomEdge, topEdge) -> cairoScope $ do
        let (p, q) = let Line p q = bottomEdge in if (q -. p) `dotProduct` Vec2 1 0 > 0 then (p, q) else (q, p)
        let shadowingSidePolys = fmap (shrinkPolygon (2*epsilon)) $ flip filter sidePolys $ \(Polygon [p', q', _, _])
                -> (_x p `between` (_x p', _x q') && cross (q' -. p') (p -. p') > 0)
                || (_x q `between` (_x p', _x q') && cross (q' -. p') (q -. p') > 0)
                || (_x p' `between` (_x p, _x q) && cross (q -. p) (p' -. p) < 0)
                || (_x q' `between` (_x p, _x q) && cross (q -. p) (q' -. p) < 0)
            shadowingTopPolys = fmap (shrinkPolygon epsilon) $ do
                (bottomPoly, topPoly) <- bottomTopPolys
                point <- [p, q]
                let testRay = Line point (point -. Vec2 0 picHeight) -- very long test ray
                let intersections = do
                        edge <- polygonEdges (shrinkPolygon epsilon bottomPoly)
                        case intersectionLL testRay edge of
                            IntersectionReal i -> pure i
                            _ -> []
                case length intersections of
                    2 -> [topPoly]
                    _ -> []
        let (fgt, bgt1, bgt2, bgt3) = clipLine (shadowingSidePolys ++ shadowingTopPolys) topEdge
            (fgb, bgb1, bgb2, bgb3) = clipLine (shadowingSidePolys ++ shadowingTopPolys) bottomEdge
        for_ (fgt ++ fgb) $ \fgl -> cairoScope $ do
            setLineWidth 4
            setColor (mma 2)
            sketch fgl
            stroke
        for_ (bgt1 ++ bgb1) $ \fgl -> cairoScope $ do
            setLineWidth 2
            setColor (mma 2 `withOpacity` 0.75)
            sketch fgl
            stroke
        for_ (bgt2 ++ bgb2 ++ bgt3 ++ bgb3) $ \fgl -> cairoScope $ do
            setLineWidth 1
            setColor (mma 2 `withOpacity` 0.5)
            sketch fgl
            stroke
  where 
    a `between` (b, c) = (b < a && a < c) || (c < a && a < b)

_x, _y :: Vec2 -> Double
_x (Vec2 x _) = x
_y (Vec2 _ y) = y

clipLine :: [Polygon] -> Line -> ([Line], [Line], [Line], [Line])
clipLine ps line = go ps [line] [] [] []
  where
    go [] fg bg1 bg2 bg3 = (fg, bg1, bg2, bg3)
    go (p : ps) fg bg1 bg2 bg3 =
        let fg' = fg >>= \l -> differenceLP l p
            bg1' = background fg' fg ++ (bg1 >>= \l -> differenceLP l p)
            bg2' = background bg1' bg1 ++ (bg2 >>= \l -> differenceLP l p)
            bg3' = background bg2' bg2 ++ (bg3 >>= \l -> differenceLP l p)
        in go ps fg' bg1' bg2' bg3'
    background fgLines fullLines = go fgLines fullLines
      where
        go [] result = result
        go (l@(Line a b) : ls) result =
            let orth = vectorOf (perpendicularBisector l) /. lineLength l
                supportingPoly = growPolygon epsilon $ Polygon [a -. epsilon *. orth, a +. epsilon *. orth, b +. epsilon *. orth, b -. epsilon *. orth]
            in  go ls (result >>= \line -> differenceLP line supportingPoly)

differenceLP :: Line -> Polygon -> [Line]
differenceLP (Line p q) scissors =
    let paper = Polygon [p, q]
        parts = differencePP paper scissors
    in fmap (head . polygonEdges . fst) parts
