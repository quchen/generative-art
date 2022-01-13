module Test.Voronoi (tests) where

import Data.Foldable
import Graphics.Rendering.Cairo as Cairo hiding (transform, x, y)

import Draw
import Geometry
import Voronoi

import Test.Common
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Voronoi Patterns"
    [ testPolygonCutting
    , testVoronoi
    ]

type MmaColor = Int

testPolygonCutting :: TestTree
testPolygonCutting = testGroup "Adding polygons"
    [ testCase "Cutting a polygon" test1
    , testCase "Adding another polygon" test2
    ]
  where
    box = Polygon [Vec2 0 0, Vec2 100 0, Vec2 100 100, Vec2 0 100]
    point1 = Vec2 20 40
    point2 = Vec2 80 90
    point3 = Vec2 60 30
    cell1 = Cell point1 box 1
    cell2 = Cell point2 box 2
    cell3 = Cell point3 box 3
    cell1' = updateCell point2 cell1
    cell1'' = updateCell point3 cell1'
    cell2' = updateCell point1 cell2
    cell2'' = updateCell point3 cell2'
    cell3' = updateCell point2 $ updateCell point1 $ cell3
    test1 = renderAllFormats 420 240 "docs/voronoi/1_cut_polygon" $ do
        Cairo.translate 10 10
        drawVoronoi [cell1]
        drawSeed cell2
        drawArrow (Vec2 110 50) (deg 0) (Distance 30)
        Cairo.translate 0 120
        drawVoronoi [cell2]
        drawSeed cell1
        drawArrow (Vec2 110 50) (deg 0) (Distance 30)
        Cairo.translate 150 (-120)
        drawVoronoi [cell1']
        drawSeed cell2
        drawArrow (Vec2 110 50) (deg 45) (Distance 30)
        Cairo.translate 0 120
        drawVoronoi [cell2']
        drawSeed cell1
        drawArrow (Vec2 110 50) (deg (-45)) (Distance 30)
        Cairo.translate 150 (-60)
        drawVoronoi [cell1', cell2']
    test2 = renderAllFormats 420 360 "docs/voronoi/2_add_polygon" $ do
        Cairo.translate 10 70
        drawVoronoi [cell1', cell2']
        drawSeed cell3
        drawArrow (Vec2 110 20) (deg (-45)) (Distance 30)
        drawArrow (Vec2 110 80) (deg 45) (Distance 30)
        Cairo.translate 0 180
        drawVoronoi [cell3]
        for_ [cell1, cell2] drawSeed
        drawArrow (Vec2 110 50) (deg 0) (Distance 30)
        Cairo.translate 150 (-240)
        drawVoronoi [cell1'']
        drawSeed cell3
        drawArrow (Vec2 110 90) (deg 45) (Distance 30)
        Cairo.translate 0 120
        drawVoronoi [cell2'']
        drawSeed cell3
        drawArrow (Vec2 110 50) (deg 0) (Distance 30)
        Cairo.translate 0 120
        drawVoronoi [cell3']
        for_ [cell1, cell2] drawSeed
        drawArrow (Vec2 110 10) (deg (-45)) (Distance 30)
        Cairo.translate 150 (-120)
        drawVoronoi [cell1'', cell2'', cell3']
    drawArrow start angle len = do
        arrowSketch (angledLine start angle len) def
        stroke

testVoronoi :: TestTree
testVoronoi = testCase "Full Voronoi pattern" test
  where
    voronoiPattern = mkVoronoi 100 100 (zip [Vec2 10 10, Vec2 80 30, Vec2 70 90, Vec2 20 99, Vec2 50 50] [1..])
    test = renderAllFormats 120 120 "docs/voronoi/3_full_voronoi" $ do
        Cairo.translate 10 10
        drawVoronoi (cells voronoiPattern)

drawVoronoi :: [VoronoiCell MmaColor] -> Render ()
drawVoronoi voronoiCells = cairoScope $ do
    setLineWidth 1
    for_ voronoiCells $ \(Cell point polygon i) -> do
        cairoScope $ do
            newPath
            polygonSketch polygon
            mmaColor i 1
            strokePreserve
            mmaColor i 0.1
            fill
        cairoScope $ do
            moveToVec (polygonAverage polygon)
            hsva 0 0 0 1
            showTextAligned HCenter VCenter (show i)
        drawPoint point i

drawSeed :: VoronoiCell MmaColor -> Render ()
drawSeed cell = drawPoint (seed cell) (props cell)

drawPoint :: Vec2 -> MmaColor -> Render ()
drawPoint point color = cairoScope $ do
    newPath
    mmaColor color 1
    circleSketch point (Distance 3)
    fill
