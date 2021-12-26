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
    face1 = VF point1 box 1
    face2 = VF point2 box 2
    face3 = VF point3 box 3
    face1' = updateFace point2 face1
    face1'' = updateFace point3 face1'
    face2' = updateFace point1 face2
    face2'' = updateFace point3 face2'
    face3' = updateFace point2 $ updateFace point1 $ face3
    test1 = renderAllFormats 420 240 "test/out/voronoi/1_cut_polygon" $ do
        Cairo.translate 10 10
        drawVoronoi [face1]
        drawCenter face2
        drawArrow (Vec2 110 50) (deg 0) (Distance 30)
        Cairo.translate 0 120
        drawVoronoi [face2]
        drawCenter face1
        drawArrow (Vec2 110 50) (deg 0) (Distance 30)
        Cairo.translate 150 (-120)
        drawVoronoi [face1']
        drawCenter face2
        drawArrow (Vec2 110 50) (deg 45) (Distance 30)
        Cairo.translate 0 120
        drawVoronoi [face2']
        drawCenter face1
        drawArrow (Vec2 110 50) (deg (-45)) (Distance 30)
        Cairo.translate 150 (-60)
        drawVoronoi [face1', face2']
    test2 = renderAllFormats 420 360 "test/out/voronoi/2_add_polygon" $ do
        Cairo.translate 10 70
        drawVoronoi [face1', face2']
        drawCenter face3
        drawArrow (Vec2 110 20) (deg (-45)) (Distance 30)
        drawArrow (Vec2 110 80) (deg 45) (Distance 30)
        Cairo.translate 0 180
        drawVoronoi [face3]
        for_ [face1, face2] drawCenter
        drawArrow (Vec2 110 50) (deg 0) (Distance 30)
        Cairo.translate 150 (-240)
        drawVoronoi [face1'']
        drawCenter face3
        drawArrow (Vec2 110 90) (deg 45) (Distance 30)
        Cairo.translate 0 120
        drawVoronoi [face2'']
        drawCenter face3
        drawArrow (Vec2 110 50) (deg 0) (Distance 30)
        Cairo.translate 0 120
        drawVoronoi [face3']
        for_ [face1, face2] drawCenter
        drawArrow (Vec2 110 10) (deg (-45)) (Distance 30)
        Cairo.translate 150 (-120)
        drawVoronoi [face1'', face2'', face3']
    drawArrow start angle len = do
        arrowSketch (angledLine start angle len) def
        stroke

testVoronoi :: TestTree
testVoronoi = testCase "Full Voronoi pattern" test
  where
    voronoiPattern = mkVoronoi 100 100 (zip [Vec2 10 10, Vec2 80 30, Vec2 70 90, Vec2 20 99, Vec2 50 50] [1..])
    test = renderAllFormats 120 120 "test/out/voronoi/3_full_voronoi" $ do
        Cairo.translate 10 10
        drawVoronoi (faces voronoiPattern)

drawVoronoi :: [VoronoiFace MmaColor] -> Render ()
drawVoronoi voronoiFaces = restoreStateAfter $ do
    setLineWidth 1
    for_ voronoiFaces $ \(VF point polygon i) -> do
        restoreStateAfter $ do
            newPath
            polygonSketch polygon
            mmaColor i 1
            strokePreserve
            mmaColor i 0.1
            fill
        restoreStateAfter $ do
            moveToVec (polygonAverage polygon)
            hsva 0 0 0 1
            showTextAligned HCenter VCenter (show i)
        drawPoint point i

drawCenter :: VoronoiFace MmaColor -> Render ()
drawCenter vf = drawPoint (center vf) (props vf)

drawPoint :: Vec2 -> MmaColor -> Render ()
drawPoint point color = restoreStateAfter $ do
    newPath
    mmaColor color 1
    circleSketch point (Distance 3)
    fill
