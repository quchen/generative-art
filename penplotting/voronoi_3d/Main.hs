{-# LANGUAGE LambdaCase #-}
module Main (main) where



import           Data.List
import           Data.Maybe
import           Data.Ord
import qualified Data.Set                 as S
import qualified Data.Vector              as V
import           Graphics.Rendering.Cairo as C hiding (height, width, x, y)
import           Math.Noise               (Perlin (..), getValue, perlin)
import           Prelude                  hiding ((**))
import           System.Random.MWC

import Draw
import Draw.Plotting
import Geometry                     as G
import Geometry.Algorithms.Delaunay
import Geometry.Algorithms.Sampling



picWidth, picHeight :: Num a => a
picWidth = 600
picHeight = 430

previewScale :: Num a => a
previewScale = 10

epsilon :: Double
epsilon = 0.01

main :: IO ()
main = do
    let count = 100

    gen <- initialize (V.fromList [12, 984, 498, 498, 626, 15, 165])
    let -- constructed so that we have roughly `count` points
        adaptiveRadius = picHeight * sqrt (0.75 / count)
        poissonShape  = boundingBox [zero, Vec2 picHeight picHeight]
        poissonRadius = adaptiveRadius
        poissonK      = 4
        bounds        = BoundingBox  (Vec2 0 0) (Vec2 picHeight picHeight)

    points <- poissonDisc gen poissonShape poissonRadius poissonK
    print (length points)
    let voronoi
            = clipCellsToBox bounds
            $ voronoiCells
            $ delaunayTriangulation
            $ V.last
            $ V.iterateN 4 (lloydRelaxation bounds 1)
            $ V.fromList points
        voronoiWithProps = (\(seed, region) -> (seed, randomHeight seed, region)) <$> zip points (V.toList voronoi)
        origin = Vec2 (picWidth/2) (picHeight/2)
        cells = sortOn (\(Polygon ps, _) -> minimum (_y <$> ps)) $ do
            (seed, height, region) <- voronoiWithProps
            let region' = G.transform
                    (  G.translate (Vec2 0 (-picHeight/5))
                    <> G.scaleAround' origin 1 0.35
                    <> G.rotateAround origin (deg 45)
                    <> G.translate (Vec2 ((picWidth - picHeight) / 2) 0 )
                    <> G.scaleAround seed 0.9 )
                    region
            pure (region', height)
        cellLines = sketchLines cells

    let plottingSettings = def
            { _feedrate = 6000
            , _zTravelHeight = 5
            , _zDrawingHeight = -2
            , _canvasBoundingBox = Just (boundingBox [zero, Vec2 picWidth picHeight])
            }
        plotResult = runPlot plottingSettings (plotCells cellLines)
    
    renderPreview "out/voronoi_3d_preview.png" previewScale plotResult
    writeGCodeFile "out/voronoi_3d.g" plotResult
    render "out/voronoi_3d.png" (previewScale * picWidth) (previewScale * picHeight) $ do
        coordinateSystem (MathStandard_ZeroBottomLeft_XRight_YUp (previewScale * picHeight))
        cairoScope (setColor white >> C.paint)
        drawCells (G.transform (G.scale previewScale) cellLines)

randomHeight :: Vec2 -> Double
randomHeight p
    = (picHeight / 6)
    + (picHeight / 2) * noise2d p
    + (picHeight / 6) * exp(- 0.000005 * normSquare (p -. origin))
  where
    noise = perlin { perlinOctaves = 4, perlinFrequency = 0.002, perlinSeed = 1980166 }
    noise2d (Vec2 x y) = fromMaybe 0 $ getValue noise (x, y, 0)
    origin = Vec2 (picHeight / 2) (picHeight / 2)

sketchLines :: [(Polygon, Double)] -> [[Line]]
sketchLines cells = 
    let bottomTopPolys = (\(p, h) -> (p, G.transform (G.translate (Vec2 0 h)) p)) <$> cells
        sidePolys = do
            (Polygon ps, height) <- cells
            (p, q) <- zip ps (drop 1 (cycle ps))
            let p1 = Polygon [p, q, q +. Vec2 0 height, p +. Vec2 0 height]
                p2 = Polygon [q, p, p +. Vec2 0 height, q +. Vec2 0 height]
            pure $ case polygonOrientation p1 of
                PolygonPositive -> p1
                PolygonNegative -> p2
        sideEdges = do
            (Polygon ps, h) <- cells
            p <- ps
            pure (Line p (p +. Vec2 0 h))
        bottomTopEdges = do
            (p, q) <- bottomTopPolys
            zip (polygonEdges p) (polygonEdges q)
        sideEdgesPlotting = sideEdges <&> \line@(Line p _) ->
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
            in  clipLine (shadowingSidePolys ++ shadowingTopPolys) line
        bottomTopEdgesPlotting = bottomTopEdges <&> \(bottomEdge, topEdge) ->
            let (p, q) = let Line p q = bottomEdge in if (q -. p) `dotProduct` Vec2 1 0 > 0 then (p, q) else (q, p)
                shadowingSidePolys = fmap (growPolygon (2*epsilon)) $ flip filter sidePolys $ \poly@(Polygon [p', q', _, _])
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
            in  zipWith (++) 
                    (clipLine (shadowingSidePolys ++ shadowingTopPolys) topEdge)
                    (clipLine (shadowingSidePolys ++ shadowingTopPolys) bottomEdge)
    in foldr (zipWith (++)) (repeat []) (sideEdgesPlotting ++ bottomTopEdgesPlotting)
  where 
    a `between` (b, c) = (b < a && a < c) || (c < a && a < b)

plotCells :: [[Line]] -> Plot ()
plotCells layers = for_ layers $ \layer -> do
    penChange
    for_ (minimizePenHoveringBy minimizePenHoveringSettings (S.fromList layer)) plot

drawCells :: [[Line]] -> C.Render ()
drawCells layers = for_ (zip layers strokes) $ \(layer, stroke) -> cairoScope $ do
    setColor black
    C.setLineWidth stroke
    sketch layer
    C.stroke
  where strokes = [4, 2, 1, 1]

_x, _y :: Vec2 -> Double
_x (Vec2 x _) = x
_y (Vec2 _ y) = y

clipLine :: [Polygon] -> Line -> [[Line]]
clipLine ps line = go ps [line] [] [] []
  where
    go [] fg bg1 bg2 bg3 = [fg, bg1, bg2, bg3]
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
differenceLP line@(Line p q) scissors = go ([p] ++ intersections ++ [q])
  where
    intersections = do
        edge <- polygonEdges scissors
        case intersectionLL line edge of
            IntersectionReal i -> pure i
            _ -> []
    go (a : b : is)
        | pointInPolygon ((a +. b) /. 2) scissors
        = go (b : is)
        | otherwise
        = Line a b : go (b : is)
    go _ = []

(<&>) :: [a] -> (a -> b) -> [b]
(<&>) = flip fmap

penChange :: Plot ()
penChange = withDrawingHeight 0 $ do
    repositionTo zero
    penDown
    pause PauseUserConfirm
    penUp

minimizePenHoveringSettings :: MinimizePenHoveringSettings Line
minimizePenHoveringSettings = MinimizePenHoveringSettings
    { _getStartEndPoint = \(Line a b) -> (a, b)
    , _flipObject = Just (\(Line a b) -> Line b a)
    , _mergeObjects = Nothing
    }
