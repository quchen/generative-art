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
    , testGroup "Travelled distance"
        [ test_travelledDistance_line
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

drawnDistance :: RunPlotResult -> Double
drawnDistance RunPlotResult{_plotInternals=TinkeringInternals{_tinkeringState=PlottingState{_drawingDistance = d}}} = d

test_plottingDistance_line :: TestTree
test_plottingDistance_line = testProperty "Line" $ \start end ->
    let line = Line start end
        plotResult = runPlot def (plot line)
    in lineLength line ~=== drawnDistance plotResult

test_plottingDistance_circle :: TestTree
test_plottingDistance_circle = testProperty "Circle" $ \center (Positive radius) ->
    let circle = Circle center radius
        plotResult = runPlot def (plot circle)
    in 2*pi*radius ~=== drawnDistance plotResult

travelledDistance :: RunPlotResult -> Double
travelledDistance RunPlotResult{_plotInternals=TinkeringInternals{_tinkeringWriterLog=PlottingWriterLog{_penTravelDistance = d}}} = d

test_travelledDistance_line :: TestTree
test_travelledDistance_line = testProperty "Line" $ \start end ->
    let plotResult = runPlot def (repositionTo start >> repositionTo end)
    in norm (start -. end) ~=== travelledDistance plotResult

drawnBB :: RunPlotResult -> BoundingBox
drawnBB RunPlotResult{_plotInternals=TinkeringInternals{_tinkeringState=PlottingState {_drawnBoundingBox = bb}}} = bb

test_boundingBox_circle :: TestTree
test_boundingBox_circle = testProperty "Circle" $ \center (Positive radius) ->
    let circle = Circle center radius
        plotResult = runPlot def (plot circle)
        expected = boundingBox [center -. Vec2 radius radius, center +. Vec2 radius radius]
    in drawnBB plotResult ~=== expected

test_boundingBox_arcCw90 :: TestTree
test_boundingBox_arcCw90 = testCase "90° (clockwise)" $ do
    let start = Vec2 100 0
        center = zero
        end = Vec2 0 (-100)
        expected = ExpectedWithin 1e-10 (boundingBox [zero, Vec2 100 (-100)])
        plotResult = runPlot def (repositionTo start >> clockwiseArcAroundTo center end)
        actual = Actual (drawnBB plotResult)
    assertApproxEqual "" expected actual

test_boundingBox_arcCw270 :: TestTree
test_boundingBox_arcCw270 = testCase "270° (clockwise)" $ do
    let start = Vec2 100 0
        center = zero
        end = Vec2 0 100
        expected = ExpectedWithin 1e-10 (boundingBox [Vec2 (-100) (-100), Vec2 100 100])
        plotResult = runPlot def (repositionTo start >> clockwiseArcAroundTo center end)
        actual = Actual (drawnBB plotResult)
    assertApproxEqual "" expected actual

test_boundingBox_arcCcw90 :: TestTree
test_boundingBox_arcCcw90 = testCase "90° (counter-clockwise)" $ do
    let start = Vec2 100 0
        center = zero
        end = Vec2 0 100
        expected = ExpectedWithin 1e-10 (boundingBox [zero, Vec2 100 100])
        plotResult = runPlot def (repositionTo start >> counterclockwiseArcAroundTo center end)
        actual = Actual (drawnBB plotResult)
    assertApproxEqual "" expected actual

test_boundingBox_arcCcw270 :: TestTree
test_boundingBox_arcCcw270 = testCase "270° (counter-clockwise)" $ do
    let start = Vec2 100 0
        center = zero
        end = Vec2 0 (-100)
        expected = ExpectedWithin 1e-10 (boundingBox [Vec2 (-100) (-100), Vec2 100 100])
        plotResult = runPlot def (repositionTo start >> counterclockwiseArcAroundTo center end)
        actual = Actual (drawnBB plotResult)
    assertApproxEqual "" expected actual
