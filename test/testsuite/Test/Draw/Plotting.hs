{-# LANGUAGE OverloadedStrings #-}

module Test.Draw.Plotting (tests) where



import Test.TastyAll

import Draw.Plotting
import Geometry      as G



tests :: TestTree
tests = testGroup "Penplotting GCode"
    [ testGroup "Drawn distance"
        [ test_plottingDistance_line
        , test_plottingDistance_circle
        ]
    , testGroup "Drawn bounding box"
        [ test_boundingBox_circle
        , testGroup "Arc"
            [ test_boundingBox_arcCw90
            , test_boundingBox_arcCw270
            , test_boundingBox_arcCcw90
            , test_boundingBox_arcCcw270
            ]
        ]
    ]

test_plottingDistance_line :: TestTree
test_plottingDistance_line = testProperty "Line" $ \start end ->
    let line = Line start end
        (_writerLog, PlottingState {_drawingDistance = drawnDistance}) = runPlotRaw def (plot line)
    in lineLength line ~=== drawnDistance

test_plottingDistance_circle :: TestTree
test_plottingDistance_circle = testProperty "Circle" $ \center (Positive radius) ->
    let circle = Circle center radius
        (_writerLog, PlottingState {_drawingDistance = drawnDistance}) = runPlotRaw def (plot circle)
    in 2*pi*radius ~=== drawnDistance

test_boundingBox_circle :: TestTree
test_boundingBox_circle = testProperty "Circle" $ \center (Positive radius) ->
    let circle = Circle center radius
        (_writerLog, PlottingState {_drawnBoundingBox = drawnBB}) = runPlotRaw def (plot circle)
        actual = drawnBB
        expected = boundingBox [center -. Vec2 radius radius, center +. Vec2 radius radius]
    in actual ~=== expected

test_boundingBox_arcCw90 :: TestTree
test_boundingBox_arcCw90 = testCase "Arc 90° (clockwise)" $ do
    let start = zero
        center = Vec2 0 100
        end = Vec2 100 100
        expected = ExpectedWithin 1e-10 (boundingBox [start, end])
        actual = Actual drawnBB
        (_writerLog, PlottingState {_drawnBoundingBox = drawnBB}) = runPlotRaw def (repositionTo start >> clockwiseArcAroundTo center end)
    assertApproxEqual "" expected actual

test_boundingBox_arcCw270 :: TestTree
test_boundingBox_arcCw270 = testCase "Arc 270° (clockwise)" $ do
    let start = zero
        center = Vec2 0 100
        end = Vec2 (-100) 100
        expected = ExpectedWithin 1e-10 (boundingBox [Vec2 (-100) 0, Vec2 100 200])
        actual = Actual drawnBB
        (_writerLog, PlottingState {_drawnBoundingBox = drawnBB}) = runPlotRaw def (repositionTo start >> clockwiseArcAroundTo center end)
    assertApproxEqual "" expected actual

test_boundingBox_arcCcw90 :: TestTree
test_boundingBox_arcCcw90 = testCase "Arc 90° (counter-clockwise)" $ do
    let start = zero
        center = Vec2 0 100
        end = Vec2 (-100) 100
        expected = ExpectedWithin 1e-10 (boundingBox [Vec2 (-100) 0, Vec2 0 100])
        actual = Actual drawnBB
        (_writerLog, PlottingState {_drawnBoundingBox = drawnBB}) = runPlotRaw def (repositionTo start >> counterclockwiseArcAroundTo center end)
    assertApproxEqual "" expected actual

test_boundingBox_arcCcw270 :: TestTree
test_boundingBox_arcCcw270 = testCase "Arc 270° (counter-clockwise)" $ do
    let start = zero
        center = Vec2 0 100
        end = Vec2 100 100
        expected = ExpectedWithin 1e-10 (boundingBox [Vec2 (-100) 0, Vec2 100 200])
        actual = Actual drawnBB
        (_writerLog, PlottingState {_drawnBoundingBox = drawnBB}) = runPlotRaw def (repositionTo start >> counterclockwiseArcAroundTo center end)
    assertApproxEqual "" expected actual
