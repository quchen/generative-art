module Test.Voronoi (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo hiding (transform, x, y)

import Draw
import Geometry
import Geometry.Shapes
import Util
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
    bounds = Polygon [Vec2 0 0, Vec2 100 0, Vec2 100 100, Vec2 0 100]
    point1 = Vec2 20 40
    point2 = Vec2 80 90
    point3 = Vec2 60 30
    face1 = VF point1 bounds 1
    face2 = VF point2 bounds 2
    face3 = VF point3 bounds 3
    face1' = updateFace face1 point2
    face1'' = updateFace face1' point3
    face2' = updateFace face2 point1
    face2'' = updateFace face2' point3
    face3' = updateFace (updateFace face3 point1) point2
    test1 = renderAllFormats 360 240 "test/out/voronoi/1_cut_polygon" $ do
        translate 10 10
        drawVoronoi [face1]
        drawCenter face2
        translate 0 120
        drawVoronoi [face2]
        drawCenter face1
        translate 120 (-120)
        drawVoronoi [face1']
        drawCenter face2
        translate 0 120
        drawVoronoi [face2']
        drawCenter face1
        translate 120 (-60)
        drawVoronoi [face1', face2']
    test2 = renderAllFormats 360 360 "test/out/voronoi/2_add_polygon" $ do
        translate 10 70
        drawVoronoi [face1', face2']
        drawCenter face3
        translate 0 180
        drawVoronoi [face3]
        for_ [face1, face2] drawCenter
        translate 120 (-240)
        drawVoronoi [face1'']
        drawCenter face3
        translate 0 120
        drawVoronoi [face2'']
        drawCenter face3
        translate 0 120
        drawVoronoi [face3']
        for_ [face1, face2] drawCenter
        translate 120 (-120)
        drawVoronoi [face1'', face2'', face3']

testVoronoi :: TestTree
testVoronoi = testCase "Full Voronoi pattern" test
  where
    voronoiPattern = voronoi (zip [Vec2 10 10, Vec2 80 30, Vec2 70 90, Vec2 20 99, Vec2 50 50] [1..]) 100 100
    test = renderAllFormats 120 120 "test/out/voronoi/3_full_voronoi" $ do
        translate 10 10
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