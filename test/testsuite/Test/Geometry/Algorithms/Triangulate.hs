module Test.Geometry.Algorithms.Triangulate (tests) where



import           Control.Monad.ST
import           Data.Foldable
import           Data.List.Extended
import           Data.Traversable
import qualified Data.Vector                     as V
import           Graphics.Rendering.Cairo        as Cairo hiding
    (transform, translate, x, y)
import qualified Graphics.Rendering.Cairo        as Cairo
import           System.Random.MWC
import           System.Random.MWC.Distributions

import Draw
import Geometry
import Geometry.Shapes

import Test.TastyAll



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
testSquare = testVisual "Square" 120 120 "docs/triangulation/1_square" $ \_ -> do
    let triangulation = triangulate (Polygon [Vec2 0 0, Vec2 100 0, Vec2 100 100, Vec2 0 100])
    Cairo.translate 10 10
    paintTriangulation triangulation

testRegular9gon :: TestTree
testRegular9gon = testVisual "Regular 9-gon" 120 120 "docs/triangulation/2_regular_polygon" $ \_ -> do
    let triangulation = triangulate (Geometry.transform (Geometry.scale 50) (regularPolygon 9))
    Cairo.translate 60 60
    paintTriangulation triangulation

testHaskellLogo :: TestTree
testHaskellLogo = testVisual "Haskell logo" 510 360 "docs/triangulation/3_haskell_logo" $ \_ -> do
    let triangulation = map triangulate wonkyHaskellLogo
        wonkyHaskellLogo = wigglePolys 5 (Geometry.transform (Geometry.scale 340) haskellLogo)
    Cairo.translate 10 10
    for_ triangulation (cairoScope . paintTriangulation)

wigglePolys :: Double -> [Polygon] -> [Polygon]
wigglePolys sigma polys = runST $ do
    gen <- initialize (V.fromList [1])
    for polys (wigglePoly gen sigma)

wigglePoly :: GenST s -> Double -> Polygon -> ST s Polygon
wigglePoly gen sigma (Polygon corners) = do
    wiggled <- for corners $ \corner -> do
        x <- normal 0 sigma gen
        y <- normal 0 sigma gen
        pure (corner +. Vec2 x y)
    pure (Polygon wiggled)

testSpiral :: TestTree
testSpiral = testVisual "Spiral" 280 260 "docs/triangulation/4_spiral" $ \_ -> do
        let triangulation = triangulate (spiralPolygon 13 20)
        Cairo.translate 130 130
        paintTriangulation triangulation

nubLines :: [Line] -> [Line]
nubLines = nubOrd . map normalize
  where
    normalize (Line v1 v2) = Line (min v1 v2) (max v1 v2)

paintTriangulation :: [Polygon] -> Render ()
paintTriangulation triangulation = do
    let colors = map mathematica97 [0..]
    for_ (zip3 [1::Int ..] colors triangulation) $ \(i, color, polygon) -> do
        cairoScope $ do
            sketch polygon
            setColor (color `withOpacity` 0.5)
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
            sketch edge
            stroke
