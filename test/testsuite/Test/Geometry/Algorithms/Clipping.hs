module Test.Geometry.Algorithms.Clipping (tests) where



import Control.Monad
import Data.Foldable
import Graphics.Rendering.Cairo as Cairo hiding (x, y)

import Draw
import Geometry.Algorithms.Clipping
import Geometry.Core           as G
import Geometry.Shapes

import Test.TastyAll



tests :: TestTree
tests = testGroup "Clipping"
    [ testGroup "Line with line"
        [ lineTest
        ]
    , testGroup "Polygon with line"
        [ cutSquareTest
        , complicatedPolygonTest
        , cutMissesPolygonTest
        , cornerCasesTests
        ]
    , testGroup "Shading"
        [ hatchRegularPolygon
        , hatchSpiralPolygon
        ]
    , testGroup "Polygon with polygon"
        [ testGroup "Intersection"
            [ intersectionOfDisjointPolygonsTest
            , intersectionWithContainedPolygon
            , intersectionOfSimpleSquaresTest
            , intersectionOfDisjointSquaresTest
            , intersectionOfDoubleTraversalSquaresTest
            , polygonIntersectionStressTest
            ]
        , testGroup "Union"
            [ unionOfSimpleSquaresTest
            , unionOfDisjointSquaresTest
            , unionOfDoubleTraversalSquaresTest
            , polygonUnionStressTest
            ]
        , testGroup "Difference"
            [ differenceOfSimpleSquaresTest
            , differenceOfDisjointSquaresTest
            , differenceOfDoubleTraversalSquaresTest
            , polygonDifferenceStressTest
            ]
        ]
    ]



lineTest :: TestTree
lineTest = testVisual "Cut my line into pieces" 220 100 "docs/geometry/clipping/1_line" $ \_ -> do
    Cairo.translate 3 32
    let paper = angledLine (Vec2 0 0) (deg 20) 100
        scissors = perpendicularBisector paper
        Cut paperStart p paperEnd = cutLineWithLine scissors paper

    setLineWidth 1
    setColor black
    setDash [2,4] 0
    sketch scissors
    stroke
    setDash [] 0

    setLineWidth 3
    setColor $ mathematica97 0
    sketch (Line paperStart p)
    stroke
    setColor $ mathematica97 3
    sketch (Line p paperEnd)
    stroke

    setColor $ mathematica97 1
    setFontSize 12
    moveTo 60 10
    showText "Cut my line in two pieces"

polyCutDraw :: Polygon -> Line -> [Polygon] -> Render ()
polyCutDraw initialPolygon scissors cutResults = do
    drawCutArrow
    drawPolygon 0 initialPolygon
    for_ (zip [0..] cutResults) (\(i, poly) -> drawPolygon i poly)
  where
    drawCutArrow = do
        setLineWidth 1
        setColor black
        setDash [2,4] 0
        sketch scissors
        stroke
        setDash [] 0
        sketch (Arrow scissors def{_arrowheadSize = 5, _arrowDrawBody = False})
        stroke
    drawPolygon i polygon = grouped paint $ do
        setColor $ mathematica97 i
        for_ (polygonEdges polygon) $ \edge -> do
            sketch (Arrow edge def
                { _arrowheadRelPos   = 0.45
                , _arrowheadSize     = 6
                , _arrowheadDrawLeft = False
                })
            stroke
        sketch polygon
        strokePreserve
        setColor $ mathematica97 i `withOpacity` 0.1
        fill

cutSquareTest :: TestTree
cutSquareTest = do
    let polygon = Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50]
        scissors = centerLine (angledLine (Vec2 25 25) (deg 20) 100)
        cutResult = cutPolygon scissors polygon

    testVisual "Convex polygon" 170 90 "docs/geometry/clipping/2_square" $ \_ -> do
        polyCutDraw
            (G.transform (G.translate (Vec2 10 10)) polygon)
            (G.transform (G.translate (Vec2 90 10)) scissors)
            (G.transform (G.translate (Vec2 90 10)) cutResult)

        setColor $ mathematica97 1
        setFontSize 12
        moveTo 90 80
        showText (show (length cutResult) ++ " polygons")

        liftIO $ do
            assertEqual "Number of resulting polygons" (Expected 2) (Actual (length cutResult))
            assertAreaConserved polygon cutResult

complicatedPolygonTest :: TestTree
complicatedPolygonTest = do
    let polygon = spiralPolygon 9 20
        scissors = centerLine (angledLine (Vec2 (-5) (-5)) (deg 140) 220)
        cutResult = cutPolygon scissors polygon

    testVisual "Concave polygon" 400 190 "docs/geometry/clipping/3_complicated" $ \_ -> do
        polyCutDraw
            (G.transform (G.translate (Vec2 90 100)) polygon)
            (G.transform (G.translate (Vec2 290 100)) scissors)
            (G.transform (G.translate (Vec2 290 100)) cutResult)

        setColor $ mathematica97 1
        setFontSize 12
        moveTo 250 15
        showText (show (length cutResult) ++ " polygons")

        liftIO $ do
            assertEqual "Number of resulting polygons" (Expected 5) (Actual (length cutResult))
            assertAreaConserved polygon cutResult

cutMissesPolygonTest :: TestTree
cutMissesPolygonTest = do
    let scissors = Line (Vec2 0 70) (Vec2 50 60)
        polygon = Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50]
        cutResult = cutPolygon scissors polygon

    testVisual "Cut misses polygon" 130 90 "docs/geometry/clipping/4_miss" $ \_ -> do
        polyCutDraw
            (G.transform (G.translate (Vec2 10 10)) polygon)
            (G.transform (G.translate (Vec2 70 10)) scissors)
            (G.transform (G.translate (Vec2 70 10)) cutResult)

        liftIO $ do
            assertEqual "Number of resulting polygons" (Expected 1) (Actual (length cutResult))
            assertAreaConserved polygon cutResult

-- These corner cases are terrible. Maybe I’ll rework the alg one day, until then I
-- don’t want to delete them, but I also do not want unused function warnings.
cornerCasesTests :: TestTree
cornerCasesTests = testGroup "Corner cases" (zigzagTest : cutThroughCornerTest : pathologicalCornerCutsTests)

zigzagTest :: TestTree
zigzagTest = testVisual "Zigzag" 150 90 "docs/geometry/clipping/5_zigzag" $ \_ -> do
    let scissors = Line (Vec2 0 25) (Vec2 50 25)
        polygon = Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 25 10, Vec2 25 50, Vec2 0 0]
        cutResult = cutPolygon scissors polygon

    polyCutDraw
        (G.transform (G.translate (Vec2 10 20)) polygon)
        (G.transform (G.translate (Vec2 80 20)) scissors)
        (G.transform (G.translate (Vec2 80 20)) cutResult)

cutThroughCornerTest :: TestTree
cutThroughCornerTest = testVisual "Cut through corner" 150 90 "docs/geometry/clipping/5_through_corner" $ \_ -> do
    let scissors = Line (Vec2 (-15) (-15)) (Vec2 65 65)
        polygon = Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50]
        cutResult = cutPolygon scissors polygon

    polyCutDraw
        (G.transform (G.translate (Vec2 10 20)) polygon)
        (G.transform (G.translate (Vec2 80 20)) scissors)
        (G.transform (G.translate (Vec2 80 20)) cutResult)

    liftIO $ do
        assertEqual "Number of resulting polygons" (Expected 2) (Actual (length cutResult))
        assertAreaConserved polygon cutResult

pathologicalCornerCutsTests :: [TestTree]
pathologicalCornerCutsTests = do
    -- Taken from https://geidav.wordpress.com/2015/03/21/splitting-an-arbitrary-polygon-by-a-line/
    (name, filenameSuffix, polygon, expectedNumPolys) <- [ooo, lol, ror, ool, oor, loo, roo]
    [ testVisual name 380 100 ("docs/geometry/clipping/6_corner_cases_" ++ filenameSuffix) $ \_ -> do
            specialCaseTest name polygon
            liftIO $ assertEqual "Expected polygons" (Expected expectedNumPolys) (Actual (length (cutPolygon scissors polygon)))
        ]
  where
    scissors = Line (Vec2 (-60) 0) (Vec2 180 0)
    specialCaseTest name polygon = cairoScope $ do
        Cairo.translate 0 50
        let cutResult = cutPolygon scissors polygon
            placeOriginal, placeCut :: Transform a => a -> a
            placeOriginal = G.transform (G.translate (Vec2 70 0))
            placeCut = G.transform (G.translate (Vec2 190 0))
        grouped paint $ do
            polyCutDraw
                (placeOriginal polygon)
                (placeOriginal scissors)
                (placeCut cutResult)
            setColor $ mathematica97 0
            for_ (let Polygon corners = polygon in corners) $ \corner -> do
                sketch (Circle (placeOriginal corner) 2.5)
                stroke
            for_ cutResult $ \cutPoly ->
                for_ (let Polygon corners = cutPoly in corners) $ \corner -> do
                    sketch (Circle (placeCut corner) 2.5)
                    stroke
        let renderDescription = do
                setColor $ mathematica97 1
                let Vec2 x y = placeCut (Vec2 0 0) in moveTo x y >> relMoveTo 70 0
                setFontSize 12
                extents <- textExtents name
                relMoveTo 0 (textExtentsHeight extents / 2)
                showText name
        renderDescription

        liftIO (assertAreaConserved polygon cutResult)

    ooo = let colinearPoints = [Vec2 (-40) 0, Vec2 0 0, Vec2 40 0]
          in ( "on → on → on"
             , "ooo"
             , Polygon (Vec2 0 40 : colinearPoints)
             , 1 )
    lol = ( "left → on → left"
          , "lol"
          , Polygon [Vec2 0 0, Vec2 40 (-40), Vec2 40 40, Vec2 (-40) 40, Vec2 (-40) (-40)]
          , 3 )
    ror = ( "right → on → right"
          , "ror"
          , Polygon [Vec2 40 40, Vec2 40 (-40), Vec2 (-40) (-40), Vec2 (-40) 40, Vec2 0 0]
          , 3 )
    ool = ( "on → on → left"
          , "ool"
          , Polygon [Vec2 0 0, Vec2 0 (-40), Vec2 40 (-40), Vec2 40 40, Vec2 (-40) 40, Vec2 (-40) 0]
          , 2 )
    oor = ( "on → on → right"
          , "oor"
          , Polygon [Vec2 0 40, Vec2 40 40, Vec2 40 (-40), Vec2 (-40) (-40), Vec2 (-40) 0, Vec2 0 0]
          , 2 )
    loo = ( "left → on → on"
          , "loo"
          , Polygon [Vec2 0 0, Vec2 40 0, Vec2 40 40, Vec2 (-40) 40, Vec2 (-40) (-40), Vec2 0 (-40)]
          , 2 )
    roo = ( "right → on → on"
          , "roo"
          , Polygon [Vec2 40 0, Vec2 40 (-40), Vec2 (-40) (-40), Vec2 (-40) 40, Vec2 0 40, Vec2 0 0]
          , 2 )

assertAreaConserved :: Polygon -> [Polygon] -> Assertion
assertAreaConserved polygon cutResult = do
    let originalArea = signedPolygonArea polygon
        sumOfCutAreas = sum (map signedPolygonArea cutResult)
    unless (originalArea ~== sumOfCutAreas) (assertFailure (unlines
        ("Total area is not conserved!"
        : map ("    " ++)
            [ "original = " ++ show originalArea
            , "Σ cuts   = " ++ show sumOfCutAreas
            , "|Δ|      = " ++ show (abs (originalArea - sumOfCutAreas)) ])))

hatchRegularPolygon :: TestTree
hatchRegularPolygon = testVisual "Regular polygon" 300 300 "docs/geometry/clipping/hatch_polygon_regular" $ \(w,h) -> do
    let polygon = G.transform (G.transformBoundingBox (regularPolygon 7) [Vec2 0 0, Vec2 w h] def) (regularPolygon 7)
        shading = hatch polygon (deg 30) 24

    setLineWidth 1
    cairoScope $ do
        setColor (mathematica97 1)
        for_ shading sketch
        stroke
    cairoScope $ do
        setColor (mathematica97 0)
        sketch polygon
        stroke

hatchSpiralPolygon :: TestTree
hatchSpiralPolygon = testVisual "Spiral polygon" 300 300 "docs/geometry/clipping/hatch_polygon_spiral" $ \(w,h) -> do
    let polygon' = spiralPolygon 9 10
        polygon = G.transform (G.transformBoundingBox polygon' [Vec2 0 0, Vec2 w h] def) polygon'
        shading = hatch polygon (deg 30) 24

    setLineWidth 1
    cairoScope $ do
        for_ (zip [1..] shading) $ \(i, line) -> setColor (mathematica97 i) >> sketch line >> stroke
    cairoScope $ do
        setColor (mathematica97 0)
        sketch polygon
        stroke

polygonBinaryOpStressTest :: (Polygon -> Polygon -> [Polygon]) -> TestName -> FilePath -> TestTree
polygonBinaryOpStressTest operation testName filePath =
    let p1 = Polygon
            [ Vec2 40 30, Vec2 140 30, Vec2 140 140, Vec2 120 140, Vec2 120 80,
            Vec2 100 80, Vec2 100 140, Vec2 80 140, Vec2 80 60, Vec2 60 60, Vec2
            60 140, Vec2 40 140 ]
        p2 = Polygon
            [ Vec2 180 20, Vec2 130 20, Vec2 130 35, Vec2 120 35, Vec2 120 20,
            Vec2 110 20, Vec2 110 35, Vec2 100 35, Vec2 100 20, Vec2 90 20, Vec2
            90 35, Vec2 80 35, Vec2 80 20, Vec2 70 20, Vec2 70 35, Vec2 60 35,
            Vec2 60 20, Vec2 20 20, Vec2 20 40, Vec2 170 60, Vec2 45 80, Vec2
            170 100, Vec2 10 120, Vec2 180 140 ]
        result = operation p1 p2

    in testVisual testName 200 150 filePath $ \_ -> do
        setLineJoin LineJoinRound
        cairoScope $ do
            setLineWidth 1
            setColor (mathematica97 0)
            sketch p1
            stroke
        cairoScope $ do
            setLineWidth 1
            setColor (mathematica97 1)
            sketch p2
            stroke
        for_ (zip [2..] result) $ \(i, polygon) -> cairoScope $ do
            setLineWidth 2
            sketch polygon
            setColor (mathematica97 i `withOpacity` 0.2)
            fill

polygonIntersectionStressTest :: TestTree
polygonIntersectionStressTest = polygonBinaryOpStressTest
    intersectionPP
    "Intersection of two complicated polygons"
    "docs/geometry/clipping/polygon-polygon-intersection"

polygonUnionStressTest :: TestTree
polygonUnionStressTest = polygonBinaryOpStressTest
    unionPP
    "Union of two complicated polygons"
    "docs/geometry/clipping/polygon-polygon-union"

polygonDifferenceStressTest :: TestTree
polygonDifferenceStressTest = polygonBinaryOpStressTest
    differencePP
    "Difference of two complicated polygons"
    "docs/geometry/clipping/polygon-polygon-difference"

intersectionOfDisjointPolygonsTest :: TestTree
intersectionOfDisjointPolygonsTest = testCase "Intersection of disjoint polygons is empty" $ do
    let p1 = boundingBoxPolygon [zero, Vec2 10 10]
        p2 = boundingBoxPolygon [Vec2 20 20, Vec2 30 30]
    assertEqual "Intersection should be empty" (Expected []) (Actual (intersectionPP p1 p2))

intersectionWithContainedPolygon :: TestTree
intersectionWithContainedPolygon = testCase "One polygon contains the other" $ do
    let large = boundingBoxPolygon [zero, Vec2 20 20]
        contained = G.transform (G.translate (Vec2 5 5)) (boundingBoxPolygon [zero, Vec2 10 10])
    assertEqual "Intersection should the inner polygon" (Expected [contained]) (Actual (intersectionPP large contained))

polygonBinaryOpSimple :: (Polygon -> Polygon -> [Polygon]) -> TestName -> FilePath -> TestTree
polygonBinaryOpSimple operation testName filePath = testVisual testName 150 150 filePath $ \_ -> do
    let p1 = boundingBoxPolygon [Vec2 10 10, Vec2 100 100]
        p2 = boundingBoxPolygon [Vec2 50 50, Vec2 140 140]
        result = operation p1 p2
    for_ (zip [0..] [p1, p2]) $ \(i, polygon) -> cairoScope $ do
        setLineWidth 1
        setColor (mathematica97 i)
        sketch polygon
        stroke
    for_ (zip [2..] result) $ \(i, polygon) -> cairoScope $ do
        setLineWidth 1
        setColor (mathematica97 i `withOpacity` 0.2)
        sketch polygon
        fill

unionOfSimpleSquaresTest :: TestTree
unionOfSimpleSquaresTest = polygonBinaryOpSimple
    unionPP
    "Union of simple squares"
    "docs/geometry/clipping/polygon-polygon-union-simple"

intersectionOfSimpleSquaresTest :: TestTree
intersectionOfSimpleSquaresTest = polygonBinaryOpSimple
    intersectionPP
    "Intersection of simple squares"
    "docs/geometry/clipping/polygon-polygon-intersection-simple"

differenceOfSimpleSquaresTest :: TestTree
differenceOfSimpleSquaresTest = polygonBinaryOpSimple
    differencePP
    "difference of simple squares"
    "docs/geometry/clipping/polygon-polygon-difference-simple"

polygonBinaryOpDisjoint :: (Polygon -> Polygon -> [Polygon]) -> TestName -> FilePath -> TestTree
polygonBinaryOpDisjoint operation testName filePath = testVisual testName 150 150 filePath $ \_ -> do
    let p1 = boundingBoxPolygon [Vec2 10 10, Vec2 70 140]
        p2 = boundingBoxPolygon [Vec2 80 10, Vec2 140 140]
        result = operation p1 p2
    for_ (zip [0..] [p1, p2]) $ \(i, polygon) -> cairoScope $ do
        setLineWidth 1
        setColor (mathematica97 i)
        sketch polygon
        stroke
    for_ (zip [2..] result) $ \(i, polygon) -> cairoScope $ do
        setLineWidth 1
        setColor (mathematica97 i `withOpacity` 0.2)
        sketch polygon
        fill

unionOfDisjointSquaresTest :: TestTree
unionOfDisjointSquaresTest = polygonBinaryOpDisjoint
    unionPP
    "Union of disjoint squares"
    "docs/geometry/clipping/polygon-polygon-union-disjoint"

intersectionOfDisjointSquaresTest :: TestTree
intersectionOfDisjointSquaresTest = polygonBinaryOpDisjoint
    intersectionPP
    "Intersection of disjoint squares"
    "docs/geometry/clipping/polygon-polygon-intersection-disjoint"

differenceOfDisjointSquaresTest :: TestTree
differenceOfDisjointSquaresTest = polygonBinaryOpDisjoint
    differencePP
    "difference of disjoint squares"
    "docs/geometry/clipping/polygon-polygon-difference-disjoint"

polygonBinaryOpDoubleTraversal :: (Polygon -> Polygon -> [Polygon]) -> TestName -> FilePath -> TestTree
polygonBinaryOpDoubleTraversal operation testName filePath = testVisual testName 150 150 filePath $ \_ -> do
    let p1 = boundingBoxPolygon [Vec2 10 50, Vec2 140 100]
        p2 = boundingBoxPolygon [Vec2 50 10, Vec2 100 140]
        result = operation p1 p2
    for_ (zip [0..] [p1, p2]) $ \(i, polygon) -> cairoScope $ do
        setLineWidth 1
        setColor (mathematica97 i)
        sketch polygon
        stroke
    for_ (zip [2..] result) $ \(i, polygon) -> cairoScope $ do
        setLineWidth 1
        setColor (mathematica97 i `withOpacity` 0.2)
        sketch polygon
        fill

unionOfDoubleTraversalSquaresTest :: TestTree
unionOfDoubleTraversalSquaresTest = polygonBinaryOpDoubleTraversal
    unionPP
    "Union of double traversal cross"
    "docs/geometry/clipping/polygon-polygon-union-double-traversal"

intersectionOfDoubleTraversalSquaresTest :: TestTree
intersectionOfDoubleTraversalSquaresTest = polygonBinaryOpDoubleTraversal
    intersectionPP
    "Intersection of double traversal cross"
    "docs/geometry/clipping/polygon-polygon-intersection-double-traversal"

differenceOfDoubleTraversalSquaresTest :: TestTree
differenceOfDoubleTraversalSquaresTest = polygonBinaryOpDoubleTraversal
    differencePP
    "difference of double traversal cross"
    "docs/geometry/clipping/polygon-polygon-difference-double-traversal"
