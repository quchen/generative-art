module Visual.Billard (tests) where

import Data.Foldable
import Graphics.Rendering.Cairo

import Draw
import Geometry

import Visual.Common
import Test.Tasty
import Test.Tasty.HUnit



tests :: TestTree
tests = testCase "Billard process" testBillard

testBillard :: IO ()
testBillard = renderAllFormats 320 240 "test/out/billard" (do
    let table = Polygon [Vec2 10 10, Vec2 310 10, Vec2 310 230, Vec2 10 230]

    billard table (Vec2 100 100) (deg $ -40)

    )
  where
    billard table startPoint startAngle = do
        let startVec = angledLine startPoint startAngle (Distance 100)
            billardPoints = startPoint : take 16 (billardProcess table startVec)

        setLineWidth 2
        hsva 0 0 0 0.5
        polygonSketch table
        stroke

        setLineWidth 1
        hsva 0 1 1 1
        for_ billardPoints (\point -> do
            circleSketch point (Distance 3)
            fill )
        hsva 180 1 0.7 1
        let billardArrows = zipWith Line billardPoints (tail billardPoints)
        for_ billardArrows (\arr -> do
            arrowSketch arr
            stroke )
