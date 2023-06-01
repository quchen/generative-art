{-# LANGUAGE OverloadedStrings #-}

module Test.Draw.Plotting (tests) where



import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Graphics.Rendering.Cairo as C
import Test.TastyAll

import Draw
import Draw.Plotting
import Geometry      as G



tests :: TestTree
tests = testGroup "Penplotting GCode"
    [ testGroup "Drawn distance"
        [ test_plottingDistance_line
        , test_plottingDistance_circle
        , test_plottingDistance_arc
        ]
    , testGroup "Travelled distance"
        [ test_travelledDistance_line
        ]
    , testGroup "Drawn bounding box"
        [ test_boundingBox_circle
        , testGroup "Arc"
            [ testGroup "Clockwise"
                [ test_boundingBox_arcCw90
                , test_boundingBox_arcCw270
                , test_boundingBox_arcCwInterQuadrantSmall
                , test_boundingBox_arcCwInterQuadrantWrapping
                ]
            , testGroup "Counter-clockwise"
                [ test_boundingBox_arcCcw90
                , test_boundingBox_arcCcw270
                , test_boundingBox_arcCcwInterQuadrantSmall
                , test_boundingBox_arcCcwInterQuadrantWrapping
                ]
            ]
        ]
    , testGroup "Pen travel optimization"
        [ test_penTravelOptimization_noFlip_noMerge
        , test_penTravelOptimization_flip_noMerge
        , test_penTravelOptimization_noFlip_merge
        , test_penTravelOptimization_flip_merge
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

test_plottingDistance_arc :: TestTree
test_plottingDistance_arc = testProperty "Arc" $ \a b d ->
    let Line p q = perpendicularBisector (Line a b)
        center = p +. d *. (q -. p)
        plotResult1 = runPlot def (repositionTo a >> clockwiseArcAroundTo center b)
        plotResult2 = runPlot def (repositionTo b >> counterclockwiseArcAroundTo center a)
    in  drawnDistance plotResult1 ~=== drawnDistance plotResult2

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
test_boundingBox_arcCw90 = testCase "90°" $ do
    let start = Vec2 100 0
        center = zero
        end = Vec2 0 (-100)
        expected = ExpectedWithin 1e-10 (boundingBox [zero, Vec2 100 (-100)])
        plotResult = runPlot def (repositionTo start >> clockwiseArcAroundTo center end)
        actual = Actual (drawnBB plotResult)
    assertApproxEqual "" expected actual

test_boundingBox_arcCw270 :: TestTree
test_boundingBox_arcCw270 = testCase "270°" $ do
    let start = Vec2 100 0
        center = zero
        end = Vec2 0 100
        expected = ExpectedWithin 1e-10 (boundingBox [Vec2 (-100) (-100), Vec2 100 100])
        plotResult = runPlot def (repositionTo start >> clockwiseArcAroundTo center end)
        actual = Actual (drawnBB plotResult)
    assertApproxEqual "" expected actual

test_boundingBox_arcCwInterQuadrantSmall :: TestTree
test_boundingBox_arcCwInterQuadrantSmall = testCase "inter-quadrant, small angle" $ do
    let start = polar (deg 60) 100
        center = zero
        end = polar (deg 30) 100
        expected = ExpectedWithin 1e-10 (boundingBox [start, end])
        plotResult = runPlot def (repositionTo start >> clockwiseArcAroundTo center end)
        actual = Actual (drawnBB plotResult)
    assertApproxEqual "" expected actual

test_boundingBox_arcCwInterQuadrantWrapping :: TestTree
test_boundingBox_arcCwInterQuadrantWrapping = testCase "inter-quadrant, wrapping angle" $ do
    let start = polar (deg 30) 100
        center = zero
        end = polar (deg 60) 100
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

test_boundingBox_arcCcwInterQuadrantSmall :: TestTree
test_boundingBox_arcCcwInterQuadrantSmall = testCase "inter-quadrant, small angle" $ do
    let start = polar (deg 30) 100
        center = zero
        end = polar (deg 60) 100
        expected = ExpectedWithin 1e-10 (boundingBox [start, end])
        plotResult = runPlot def (repositionTo start >> counterclockwiseArcAroundTo center end)
        actual = Actual (drawnBB plotResult)
    assertApproxEqual "" expected actual

test_boundingBox_arcCcwInterQuadrantWrapping :: TestTree
test_boundingBox_arcCcwInterQuadrantWrapping = testCase "inter-quadrant, wrapping angle" $ do
    let start = polar (deg 60) 100
        center = zero
        end = polar (deg 30) 100
        expected = ExpectedWithin 1e-10 (boundingBox [Vec2 (-100) (-100), Vec2 100 100])
        plotResult = runPlot def (repositionTo start >> counterclockwiseArcAroundTo center end)
        actual = Actual (drawnBB plotResult)
    assertApproxEqual "" expected actual

test_penTravelOptimization_noFlip_noMerge :: TestTree
test_penTravelOptimization_noFlip_noMerge = testVisual "Pen travel optimization without flipping and merging" 200 200 "docs/plotting/penTravel_noFlip_noMerge" $
    let settings = MinimizePenHoveringSettings
            { _getStartEndPoint = \xs -> (V.head xs, V.last xs)
            , _flipObject = Nothing
            , _mergeObjects = Nothing
            }
    in  testPenTravelOptimization settings

test_penTravelOptimization_flip_noMerge :: TestTree
test_penTravelOptimization_flip_noMerge = testVisual "Pen travel optimization with flipping, no merging" 200 200 "docs/plotting/penTravel_flip_noMerge" $
    let settings = MinimizePenHoveringSettings
            { _getStartEndPoint = \xs -> (V.head xs, V.last xs)
            , _flipObject = Just V.reverse
            , _mergeObjects = Nothing
            }
    in  testPenTravelOptimization settings

test_penTravelOptimization_noFlip_merge :: TestTree
test_penTravelOptimization_noFlip_merge = testVisual "Pen travel optimization with merging, no flipping" 200 200 "docs/plotting/penTravel_noFlip_merge" $
    let settings = MinimizePenHoveringSettings
            { _getStartEndPoint = \xs -> (V.head xs, V.last xs)
            , _flipObject = Nothing
            , _mergeObjects = Just $ \a b -> if V.last a == V.head b then Just (a <> b) else Nothing
            }
    in  testPenTravelOptimization settings

test_penTravelOptimization_flip_merge :: TestTree
test_penTravelOptimization_flip_merge = testVisual "Pen travel optimization with flipping and merging" 200 200 "docs/plotting/penTravel_flip_merge" $
    let settings = MinimizePenHoveringSettings
            { _getStartEndPoint = \xs -> (V.head xs, V.last xs)
            , _flipObject = Just V.reverse
            , _mergeObjects = Just $ \a b -> if V.last a == V.head b then Just (a <> b) else Nothing
            }
    in  testPenTravelOptimization settings

testPenTravelOptimization :: MinimizePenHoveringSettings (V.Vector Vec2) -> (Double, Double) -> C.Render ()
testPenTravelOptimization settings (w, h) = do
    coordinateSystem (MathStandard_ZeroCenter_XRight_YUp w h)
    let result = runPlot def { _previewDecorate = False } $ plot (Polyline . toList<$> minimizePenHoveringBy settings penTravelOptimizationExample)
    _plotPreview result

penTravelOptimizationExample :: S.Set (V.Vector Vec2)
penTravelOptimizationExample = S.fromList $ V.fromList <$>
    [ [Vec2 (-80) (-80), Vec2 (-80) 0]
    , [Vec2 (-80) 0, Vec2 80 0]
    , [Vec2 80 80, Vec2 80 0]
    , [Vec2 (-80) 20, Vec2 (-80) 80]
    , [Vec2 (-60) 80, Vec2 (-60) 50]
    , [Vec2 (-40) 50, Vec2 (-40) 80]
    , [Vec2 (-40) 50, Vec2 (-40) 20]
    , [Vec2 (-20) 80, Vec2 (-20) 50]
    , [Vec2 (-20) 50, Vec2 (-20) 20]
    , [Vec2 0 20, Vec2 0 50]
    ]
