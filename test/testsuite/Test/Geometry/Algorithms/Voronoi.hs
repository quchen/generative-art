module Test.Geometry.Algorithms.Voronoi (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo as Cairo hiding (transform, x, y)

import Draw
import Geometry
import Geometry.Algorithms.Voronoi

import Test.TastyAll



tests :: TestTree
tests = testGroup "Voronoi Patterns"
    [ testPolygonCutting
    , testVoronoi
    ]

type MathematicaColor = Int

testPolygonCutting :: TestTree
testPolygonCutting = testGroup "Adding polygons"
    [ test1
    , test2
    ]
  where
    box = Polygon [Vec2 0 0, Vec2 100 0, Vec2 100 100, Vec2 0 100]
    point1 = Vec2 20 40
    point2 = Vec2 80 90
    point3 = Vec2 60 30
    cell1 = VoronoiCell point1 box 1
    cell2 = VoronoiCell point2 box 2
    cell3 = VoronoiCell point3 box 3
    cell1' = updateCell point2 cell1
    cell1'' = updateCell point3 cell1'
    cell2' = updateCell point1 cell2
    cell2'' = updateCell point3 cell2'
    cell3' = updateCell point2 $ updateCell point1 cell3
    test1 = testVisual "Cutting a polygon" 420 240 "docs/voronoi/1_cut_polygon" $ \_ -> do
        Cairo.translate 10 10
        drawVoronoi [cell1]
        drawSeed cell2
        drawArrow (Vec2 110 50) (deg 0) 30
        Cairo.translate 0 120
        drawVoronoi [cell2]
        drawSeed cell1
        drawArrow (Vec2 110 50) (deg 0) 30
        Cairo.translate 150 (-120)
        drawVoronoi [cell1']
        drawSeed cell2
        drawArrow (Vec2 110 50) (deg 45) 30
        Cairo.translate 0 120
        drawVoronoi [cell2']
        drawSeed cell1
        drawArrow (Vec2 110 50) (deg (-45)) 30
        Cairo.translate 150 (-60)
        drawVoronoi [cell1', cell2']
    test2 = testVisual "Adding another polygon" 420 360 "docs/voronoi/2_add_polygon" $ \_ -> do
        Cairo.translate 10 70
        drawVoronoi [cell1', cell2']
        drawSeed cell3
        drawArrow (Vec2 110 20) (deg (-45)) 30
        drawArrow (Vec2 110 80) (deg 45) 30
        Cairo.translate 0 180
        drawVoronoi [cell3]
        for_ [cell1, cell2] drawSeed
        drawArrow (Vec2 110 50) (deg 0) 30
        Cairo.translate 150 (-240)
        drawVoronoi [cell1'']
        drawSeed cell3
        drawArrow (Vec2 110 90) (deg 45) 30
        Cairo.translate 0 120
        drawVoronoi [cell2'']
        drawSeed cell3
        drawArrow (Vec2 110 50) (deg 0) 30
        Cairo.translate 0 120
        drawVoronoi [cell3']
        for_ [cell1, cell2] drawSeed
        drawArrow (Vec2 110 10) (deg (-45)) 30
        Cairo.translate 150 (-120)
        drawVoronoi [cell1'', cell2'', cell3']
    drawArrow start angle len = do
        sketch (Arrow (angledLine start angle len) def)
        stroke

testVoronoi :: TestTree
testVoronoi = testVisual "Full Voronoi pattern" 120 120 "docs/voronoi/3_full_voronoi" $ \_ -> do
    let voronoiPattern = mkVoronoi 100 100 (zip [Vec2 10 10, Vec2 80 30, Vec2 70 90, Vec2 20 99, Vec2 50 50] [1..])
    Cairo.translate 10 10
    drawVoronoi (cells voronoiPattern)

drawVoronoi :: [VoronoiCell MathematicaColor] -> Render ()
drawVoronoi voronoiCells = cairoScope $ do
    setLineWidth 1
    for_ voronoiCells $ \(VoronoiCell point polygon i) -> do
        cairoScope $ do
            newPath
            sketch polygon
            setColor $ mathematica97 i
            strokePreserve
            setColor $ mathematica97 i `withOpacity` 0.1
            fill
        cairoScope $ do
            moveToVec (polygonAverage polygon)
            setColor black
            showTextAligned HCenter VCenter (show i)
        drawPoint point i

drawSeed :: VoronoiCell MathematicaColor -> Render ()
drawSeed cell = drawPoint (seed cell) (props cell)

drawPoint :: Vec2 -> MathematicaColor -> Render ()
drawPoint point color = cairoScope $ do
    newPath
    setColor $ mathematica97 color
    sketch (Circle point 3)
    fill
