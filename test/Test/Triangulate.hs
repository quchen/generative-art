module Test.Triangulate (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo as Cairo hiding (transform, x, y)
import System.Random

import Draw
import Geometry
import Geometry.Shapes
import Util

import Test.Common
import Test.Tasty
import Test.Tasty.HUnit



tests :: TestTree
tests = testGroup "Polygon triangulation"
    [ testGroup "Visual"
        [ testSquare
        , testRegular9gon
        , testHaskellLogo
        , testSpiral
        ]
    ]

testSquare :: TestTree
testSquare = testCase "Square" test
  where
    triangulation = triangulate (Polygon [Vec2 0 0, Vec2 100 0, Vec2 100 100, Vec2 0 100])
    test = renderAllFormats 120 120 "test/out/triangulation/1_square" $ do
        Cairo.translate 10 10
        paintTriangulation triangulation

testRegular9gon :: TestTree
testRegular9gon = testCase "Regular 9-gon" test
  where
    triangulation = triangulate (Geometry.scale 50 (regularPolygon 9))
    test = renderAllFormats 120 120 "test/out/triangulation/2_regular_polygon" $ do
        Cairo.translate 60 60
        paintTriangulation triangulation

testHaskellLogo :: TestTree
testHaskellLogo = testCase "Haskell logo" test
  where
    triangulation = map triangulate wonkyHaskellLogo
    test = renderAllFormats 510 360 "test/out/triangulation/3_haskell_logo" $ do
        Cairo.translate 10 10
        for_ triangulation (restoreStateAfter . paintTriangulation)

    wonkyHaskellLogo :: [Polygon]
    wonkyHaskellLogo = map wigglePoly (Geometry.scale 340 haskellLogo)
      where
        wigglePoly :: Polygon -> Polygon
        wigglePoly (Polygon corners) = Polygon (map wiggle corners)
        wiggle :: Vec2 -> Vec2
        wiggle v@(Vec2 x y)
          = let seed = let (x1,x2) = decodeFloat x
                           (y1,y2) = decodeFloat y
                       in fromIntegral x1 + x2 + fromIntegral y1 + y2 + 1
                (angle, _gen') = randomR (0, 360) (mkStdGen seed)
            in moveRad (Angle angle) (Distance 10) v

testSpiral :: TestTree
testSpiral = testCase "Spiral" test
  where
    triangulation = triangulate (spiralPolygon 13 20)
    test = renderAllFormats 280 260 "test/out/triangulation/4_spiral" $ do
        Cairo.translate 130 130
        paintTriangulation triangulation

nubLines :: [Line] -> [Line]
nubLines = nub' . map normalize
  where
    normalize (Line v1 v2) = Line (min v1 v2) (max v1 v2)

paintTriangulation :: [Polygon] -> Render ()
paintTriangulation triangulation = do
    let setColors = map (\c -> mmaColor c) [0..]
    for_ (zip3 [1::Int ..] setColors triangulation) $ \(i, setColor, polygon) -> do
        restoreStateAfter $ do
            polygonSketch polygon
            setColor 0.5
            fill
        restoreStateAfter $ do
            moveToVec (polygonAverage polygon)
            hsva 0 0 0 1
            showTextAligned HCenter VCenter (show i)
    restoreStateAfter $ do
        setLineWidth 0.5
        hsva 0 0 0 1
        let allEdges = polygonEdges =<< triangulation
            uniqueEdges = nubLines allEdges
        for_ uniqueEdges $ \edge -> do
            lineSketch edge
            stroke
