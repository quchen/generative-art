{-# LANGUAGE OverloadedStrings #-}

module Test.Draw.Plotting (tests) where



import Test.TastyAll

import Draw.Plotting
import Geometry      as G



tests :: TestTree
tests = testGroup "Penplotting GCode"
    [ testGroup "Plotting distance"
        [ test_plottingDistanceOfLine
        ]
    ]

test_plottingDistanceOfLine :: TestTree
test_plottingDistanceOfLine = testProperty "Line" $ \start end ->
    let line = Line start end
        (_gcode, (PlottingState {_drawingDistance = drawnDistance}, _)) = runPlotRaw def (plot line)
    in lineLength line ~=== drawnDistance
