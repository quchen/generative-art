{-# LANGUAGE OverloadedStrings #-}

module Test.Draw.Plotting (tests) where



import Test.TastyAll

import Draw.Plotting
import Geometry      as G



tests :: TestTree
tests = testGroup "Penplotting GCode"
    [ testGroup "Drawn distance"
        [ test_plottingDistanceOfLine
        , test_plottingDistance_circle
        ]
    , testGroup "Drawn bounding box"
        [ test_boundingBox_circle
        ]
    ]

test_plottingDistanceOfLine :: TestTree
test_plottingDistanceOfLine = testProperty "Line" $ \start end ->
    let line = Line start end
        (_gcode, (PlottingState {_drawingDistance = drawnDistance}, _)) = runPlotRaw def (plot line)
    in lineLength line ~=== drawnDistance

test_plottingDistance_circle :: TestTree
test_plottingDistance_circle = testProperty "Circle" $ \center (Positive radius) ->
    let circle = Circle center radius
        (_gcode, (PlottingState {_drawingDistance = drawnDistance}, _)) = runPlotRaw def (repositionTo center >> plot circle)
    in 2*pi*radius ~=== drawnDistance

test_boundingBox_circle :: TestTree
test_boundingBox_circle = testCase "Circle" $ do
    let center = zero
        radius = 100
        circle = Circle center radius
        (_gcode, (_state, drawnBB)) = runPlotRaw def (repositionTo center >> plot circle)
    assertApproxEqual "Bounding box" (ExpectedWithin 1e-10 (boundingBox circle)) (Actual drawnBB)
