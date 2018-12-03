module Test.RandomCut (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo hiding (x, y)
import System.Random
import qualified Data.Set as S

import Draw
import Geometry
import Geometry.Processes.RandomCut

import Test.Common
import Test.Tasty
import Test.Tasty.HUnit



tests :: TestTree
tests = testGroup "Random Cut"
    [ testSquare
    ]

testSquare :: TestTree
testSquare = testCase "Square" $ renderAllFormats 220 220 "test/out/cut_random/1_square" $ do
    let gen = mkStdGen 0
        initialPolygon = Polygon [Vec2 0 0, Vec2 200 0, Vec2 200 200, Vec2 0 200]
        recurse polygon = minMaxAreaRatio [polygon, initialPolygon] >= 1/30
        accept polys = minMaxAreaRatio polys >= 1/3
        (cutResult, _gen') = randomCutProcess recurse accept initialPolygon gen

    let setColors = map mmaColor [0..]
    setLineWidth 1
    translate 10 10
    restoreStateAfter $ do
        for_ (S.fromList (cutResult >>= polygonEdges)) $ \edge -> do
            lineSketch edge
        hsva 0 0 0 1
        setLineWidth 0.5
        stroke
    restoreStateAfter $ for_ (zip setColors cutResult) $ \(setColor, polygon) -> do
        polygonSketch polygon
        setColor 0.5
        fill
