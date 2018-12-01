module Test.RandomCut (tests) where



import Graphics.Rendering.Cairo hiding (x, y)
import Data.Foldable
import System.Random

import Draw
import Geometry
import Geometry.Processes.RandomCut

import Test.Common
import Test.Tasty
import Test.Tasty.HUnit



tests :: TestTree
tests = testGroup "Random Cut"
    [ testSquare
    , testSquareIterated
    ]

testSquare :: TestTree
testSquare = testCase "Square" $ renderAllFormats 580 480 "test/out/cut_random/1_square" $ do
    let gen = mkStdGen 0
        initialPolygon = Polygon [Vec2 0 0, Vec2 100 0, Vec2 100 100, Vec2 0 100]
        (cutResult, _gen') = cutProcess gen initialPolygon

    let setColors = map mmaColor [0..]
    setLineWidth 1
    translate 10 10
    for_ (zip setColors cutResult) $ \(setColor, polygon) -> do
        polygonSketch polygon
        setColor 1
        strokePreserve
        setColor 0.2
        fill

testSquareIterated :: TestTree
testSquareIterated = testCase "Square, iterated" $ renderAllFormats 580 480 "test/out/cut_random/2_square_iterated" $ do
    let gen = mkStdGen 0
        initialPolygon = Polygon [Vec2 0 0, Vec2 100 0, Vec2 100 100, Vec2 0 100]
        (cutResult, _gen') = cutProcess gen initialPolygon

    let setColors = map mmaColor [0..]
    setLineWidth 1
    translate 10 10
    for_ (zip setColors cutResult) $ \(setColor, polygon) -> do
        polygonSketch polygon
        setColor 1
        strokePreserve
        setColor 0.2
        fill
