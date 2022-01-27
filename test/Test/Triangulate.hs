module Test.Triangulate (tests) where



import           Data.Foldable
import           Graphics.Rendering.Cairo as Cairo hiding
    (transform, translate, x, y)
import qualified Graphics.Rendering.Cairo as Cairo
import           Control.Monad.ST
import           System.Random.MWC.Distributions
import           System.Random.MWC
import Data.Traversable
import Control.Monad

import Draw
import Geometry
import Geometry.Shapes
import Geometry.Chaotic
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
    test = renderAllFormats 120 120 "docs/triangulation/1_square" $ do
        Cairo.translate 10 10
        paintTriangulation triangulation

testRegular9gon :: TestTree
testRegular9gon = testCase "Regular 9-gon" test
  where
    triangulation = triangulate (Geometry.transform (Geometry.scale 50) (regularPolygon 9))
    test = renderAllFormats 120 120 "docs/triangulation/2_regular_polygon" $ do
        Cairo.translate 60 60
        paintTriangulation triangulation

testHaskellLogo :: TestTree
testHaskellLogo = testCase "Haskell logo" test
  where
    triangulation = map triangulate wonkyHaskellLogo
    test = renderAllFormats 510 360 "docs/triangulation/3_haskell_logo" $ do
        Cairo.translate 10 10
        for_ triangulation (cairoScope . paintTriangulation)

    wonkyHaskellLogo :: [Polygon]
    wonkyHaskellLogo = map (wigglePoly 3) (Geometry.transform (Geometry.scale 340) haskellLogo)

wigglePoly :: Double -> Polygon -> Polygon
wigglePoly sigma (Polygon corners) = runST $ do
    gen <- initializeMwc ("perturb the rng a little", sigma, corners)
    warmup <- replicateM 1000 (uniformM gen)
    let _ = warmup :: [Int]
    wiggled <- for corners $ \corner -> do
        x <- normal 0 sigma gen
        y <- normal 0 sigma gen
        pure (corner +. Vec2 x y)
    pure (Polygon wiggled)


testSpiral :: TestTree
testSpiral = testCase "Spiral" test
  where
    triangulation = triangulate (spiralPolygon 13 20)
    test = renderAllFormats 280 260 "docs/triangulation/4_spiral" $ do
        Cairo.translate 130 130
        paintTriangulation triangulation

nubLines :: [Line] -> [Line]
nubLines = nubOrd . map normalize
  where
    normalize (Line v1 v2) = Line (min v1 v2) (max v1 v2)

paintTriangulation :: [Polygon] -> Render ()
paintTriangulation triangulation = do
    let colors = map mmaColor [0..]
    for_ (zip3 [1::Int ..] colors triangulation) $ \(i, color, polygon) -> do
        cairoScope $ do
            polygonSketch polygon
            setColor (color 0.5)
            fill
        cairoScope $ do
            moveToVec (polygonAverage polygon)
            setColor black
            showTextAligned HCenter VCenter (show i)
    cairoScope $ do
        setLineWidth 0.5
        setColor black
        let allEdges = polygonEdges =<< triangulation
            uniqueEdges = nubLines allEdges
        for_ uniqueEdges $ \edge -> do
            lineSketch edge
            stroke
