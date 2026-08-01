module Test.Geometry.Algorithms.Clipping (tests) where



import Control.Monad
import Data.Foldable
import Graphics.Rendering.Cairo as Cairo hiding (x, y)

import Draw
import Geometry.Algorithms.Clipping
import Geometry.Core                as G
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
            , intersectionOfSimpleSquaresTest
            , intersectionOfDisjointSquaresTest
            , intersectionOfDoubleTraversalSquaresTest
            , polygonIntersectionStressTest
            , intersectionOfFullySubsumedSquaresTest
            ]
        , testGroup "Union"
            [ unionOfSimpleSquaresTest
            , unionOfDisjointSquaresTest
            , unionOfDoubleTraversalSquaresTest
            , polygonUnionStressTest
            , unionOfFullySubsumedSquaresTest
            ]
        , testGroup "Difference"
            [ differenceOfSimpleSquaresTest
            , differenceOfDisjointSquaresTest
            , differenceOfDoubleTraversalSquaresTest
            , polygonDifferenceStressTest
            , differenceOfFullySubsumedSquaresTest
            ]
        , testGroup "Corner cases (vertex-on-edge / shared-edge)"
            [ sharedCornerUnionTest
            , sharedCornerIntersectionTest
            , sharedCornerDifferenceTest
            , vertexOnEdgeUnionTest
            , vertexOnEdgeIntersectionTest
            , vertexOnEdgeDifferenceTest
            , sharedEdgeUnionTest
            , sharedEdgeIntersectionTest
            , sharedEdgeDifferenceTest
            ]
        , testGroup "Regression cases (FP-divergent shared vertex / edge)"
            [ failure1, failure2, failure3, failure4, failure5, failure6 ]
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
    setColor $ mma 0
    sketch (Line paperStart p)
    stroke
    setColor $ mma 3
    sketch (Line p paperEnd)
    stroke

    setColor $ mma 1
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
        setColor $ mma i
        for_ (polygonEdges polygon) $ \edge -> do
            sketch (Arrow edge def
                { _arrowheadRelPos   = 0.45
                , _arrowheadSize     = 6
                , _arrowheadDrawLeft = False
                })
            stroke
        sketch polygon
        strokePreserve
        setColor $ mma i `withOpacity` 0.1
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

        setColor $ mma 1
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

        setColor $ mma 1
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
            setColor $ mma 0
            for_ (let Polygon corners = polygon in corners) $ \corner -> do
                sketch (Circle (placeOriginal corner) 2.5)
                stroke
            for_ cutResult $ \cutPoly ->
                for_ (let Polygon corners = cutPoly in corners) $ \corner -> do
                    sketch (Circle (placeCut corner) 2.5)
                    stroke
        let renderDescription = do
                setColor $ mma 1
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
        shading = hatch polygon (deg 30) 24 0

    setLineWidth 1
    cairoScope $ do
        setColor (mma 1)
        for_ shading sketch
        stroke
    cairoScope $ do
        setColor (mma 0)
        sketch polygon
        stroke

hatchSpiralPolygon :: TestTree
hatchSpiralPolygon = testVisual "Spiral polygon" 300 300 "docs/geometry/clipping/hatch_polygon_spiral" $ \(w,h) -> do
    let polygon' = spiralPolygon 9 10
        polygon = G.transform (G.transformBoundingBox polygon' [Vec2 0 0, Vec2 w h] def) polygon'
        shading = hatch polygon (deg 30) 24 0

    setLineWidth 1
    cairoScope $ do
        for_ (zip [1..] shading) $ \(i, line) -> setColor (mma i) >> sketch line >> stroke
    cairoScope $ do
        setColor (mma 0)
        sketch polygon
        stroke

polygonBinaryOpRender :: Polygon -> Polygon -> [(Polygon, IslandOrHole)] -> Render ()
polygonBinaryOpRender p1 p2 result = do
    setLineJoin LineJoinRound
    cairoScope $ setColor (mma 0) >> setLineWidth 1 >> sketch p1 >> stroke
    cairoScope $ setColor (mma 1) >> setLineWidth 1 >> sketch p2 >> stroke
    cairoScope $ do
        for_ (zip [2..] result) $ \(i, (polygon, ty)) -> cairoScope $ do
            cairoScope $ do
                setLineWidth 1
                sketch polygon
                setColor (mma i `withOpacity` 0.2)
                fillPreserve
                setColor black
                case ty of
                    Island -> pure ()
                    Hole -> setDash [2,2] 0
                stroke
            let Polygon corners = polygon
            for_ corners $ \corner -> cairoScope $ do
                sketch (Circle corner 1.5)
                fill

polygonBinaryOpStressTest :: (Polygon -> Polygon -> [(Polygon, IslandOrHole)]) -> TestName -> FilePath -> TestTree
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

    in testVisual testName 200 150 filePath $ \_ -> polygonBinaryOpRender p1 p2 result

polygonIntersectionStressTest :: TestTree
polygonIntersectionStressTest = polygonBinaryOpStressTest
    intersectionPP
    "Intersection of two complicated polygons"
    "docs/geometry/clipping/polygon-polygon-intersection-complicated"

polygonUnionStressTest :: TestTree
polygonUnionStressTest = polygonBinaryOpStressTest
    unionPP
    "Union of two complicated polygons"
    "docs/geometry/clipping/polygon-polygon-union-complicated"

polygonDifferenceStressTest :: TestTree
polygonDifferenceStressTest = polygonBinaryOpStressTest
    differencePP
    "Difference of two complicated polygons"
    "docs/geometry/clipping/polygon-polygon-difference-complicated"

intersectionOfDisjointPolygonsTest :: TestTree
intersectionOfDisjointPolygonsTest = testCase "Intersection of disjoint polygons is empty" $ do
    let p1 = boundingBoxPolygon [zero, Vec2 10 10]
        p2 = boundingBoxPolygon [Vec2 20 20, Vec2 30 30]
    assertEqual "Intersection should be empty" (Expected []) (Actual (intersectionPP p1 p2))

polygonBinaryOpSimple :: (Polygon -> Polygon -> [(Polygon, IslandOrHole)]) -> TestName -> FilePath -> TestTree
polygonBinaryOpSimple operation testName filePath =
    let p1 = boundingBoxPolygon [Vec2 10 10, Vec2 100 100]
        p2 = boundingBoxPolygon [Vec2 50 50, Vec2 140 140]
        result = operation p1 p2
    in testVisual testName 150 150 filePath $ \_ -> polygonBinaryOpRender p1 p2 result

unionOfSimpleSquaresTest :: TestTree
unionOfSimpleSquaresTest = polygonBinaryOpSimple
    unionPP
    "Simple squares"
    "docs/geometry/clipping/polygon-polygon-union-simple"

intersectionOfSimpleSquaresTest :: TestTree
intersectionOfSimpleSquaresTest = polygonBinaryOpSimple
    intersectionPP
    "Simple squares"
    "docs/geometry/clipping/polygon-polygon-intersection-simple"

differenceOfSimpleSquaresTest :: TestTree
differenceOfSimpleSquaresTest = polygonBinaryOpSimple
    differencePP
    "Simple squares"
    "docs/geometry/clipping/polygon-polygon-difference-simple"

polygonBinaryOpDisjoint :: (Polygon -> Polygon -> [(Polygon, IslandOrHole)]) -> TestName -> FilePath -> TestTree
polygonBinaryOpDisjoint operation testName filePath =
    let p1 = boundingBoxPolygon [Vec2 10 10, Vec2 70 140]
        p2 = boundingBoxPolygon [Vec2 80 10, Vec2 140 140]
        result = operation p1 p2
    in testVisual testName 150 150 filePath $ \_ -> polygonBinaryOpRender p1 p2 result

unionOfDisjointSquaresTest :: TestTree
unionOfDisjointSquaresTest = polygonBinaryOpDisjoint
    unionPP
    "Disjoint squares"
    "docs/geometry/clipping/polygon-polygon-union-disjoint"

intersectionOfDisjointSquaresTest :: TestTree
intersectionOfDisjointSquaresTest = polygonBinaryOpDisjoint
    intersectionPP
    "Disjoint squares"
    "docs/geometry/clipping/polygon-polygon-intersection-disjoint"

differenceOfDisjointSquaresTest :: TestTree
differenceOfDisjointSquaresTest = polygonBinaryOpDisjoint
    differencePP
    "Disjoint squares"
    "docs/geometry/clipping/polygon-polygon-difference-disjoint"

polygonBinaryOpDoubleTraversal :: (Polygon -> Polygon -> [(Polygon, IslandOrHole)]) -> TestName -> FilePath -> TestTree
polygonBinaryOpDoubleTraversal operation testName filePath =
    let p1 = boundingBoxPolygon [Vec2 10 50, Vec2 140 100]
        p2 = boundingBoxPolygon [Vec2 50 10, Vec2 100 140]
        result = operation p1 p2
    in testVisual testName 150 150 filePath $ \_ -> polygonBinaryOpRender p1 p2 result

unionOfDoubleTraversalSquaresTest :: TestTree
unionOfDoubleTraversalSquaresTest = polygonBinaryOpDoubleTraversal
    unionPP
    "Double traversal cross"
    "docs/geometry/clipping/polygon-polygon-union-double-traversal"

intersectionOfDoubleTraversalSquaresTest :: TestTree
intersectionOfDoubleTraversalSquaresTest = polygonBinaryOpDoubleTraversal
    intersectionPP
    "Double traversal cross"
    "docs/geometry/clipping/polygon-polygon-intersection-double-traversal"

differenceOfDoubleTraversalSquaresTest :: TestTree
differenceOfDoubleTraversalSquaresTest = polygonBinaryOpDoubleTraversal
    differencePP
    "Double traversal cross"
    "docs/geometry/clipping/polygon-polygon-difference-double-traversal"

polygonBinaryOpFullySubsumed :: (Polygon -> Polygon -> [(Polygon, IslandOrHole)]) -> TestName -> FilePath -> TestTree
polygonBinaryOpFullySubsumed operation testName filePath =
    let p1 = boundingBoxPolygon [Vec2 10 10, Vec2 90 90]
        p2 = boundingBoxPolygon [Vec2 40 40, Vec2 60 60]
        result = operation p1 p2
    in testVisual testName 100 100 filePath $ \_ -> polygonBinaryOpRender p1 p2 result

unionOfFullySubsumedSquaresTest :: TestTree
unionOfFullySubsumedSquaresTest = polygonBinaryOpFullySubsumed
    unionPP
    "Second polygon is fully inside first"
    "docs/geometry/clipping/polygon-polygon-union-fully-subsumed"

intersectionOfFullySubsumedSquaresTest :: TestTree
intersectionOfFullySubsumedSquaresTest = polygonBinaryOpFullySubsumed
    intersectionPP
    "Second polygon is fully inside first"
    "docs/geometry/clipping/polygon-polygon-intersection-fully-subsumed"

differenceOfFullySubsumedSquaresTest :: TestTree
differenceOfFullySubsumedSquaresTest = polygonBinaryOpFullySubsumed
    differencePP
    "Second polygon is fully inside first"
    "docs/geometry/clipping/polygon-polygon-difference-fully-subsumed"


-- | Two axis-aligned squares sharing exactly one corner. The doc examples use
-- @boundingBoxPolygon [Vec2 10 10, Vec2 100 100]@ and
-- @boundingBoxPolygon [Vec2 50 50, Vec2 140 140]@, which share the corner
-- @(50,50)@. Before the fix this triggered
-- @Multwomap: Overflow: second arg already has two targets@ because the shared
-- coordinate ended up with two outgoing fragments in one polygon's fragment map
-- and only one in the other's, and 'MM.union' then overflowed. With the fix,
-- 'cutPolygon' drops the redundant boundary entry and the fragments chain up
-- correctly.
sharedCornerUnionTest :: TestTree
sharedCornerUnionTest = polygonBinaryOpSimple
    unionPP
    "Shared corner (union)"
    "docs/geometry/clipping/polygon-polygon-union-shared-corner"

sharedCornerIntersectionTest :: TestTree
sharedCornerIntersectionTest = polygonBinaryOpSimple
    intersectionPP
    "Shared corner (intersection)"
    "docs/geometry/clipping/polygon-polygon-intersection-shared-corner"

sharedCornerDifferenceTest :: TestTree
sharedCornerDifferenceTest = polygonBinaryOpSimple
    differencePP
    "Shared corner (difference)"
    "docs/geometry/clipping/polygon-polygon-difference-shared-corner"

-- | A vertex of one polygon lies exactly on an edge of the other (but no shared
-- corner). Constructed so the bottom edge of p1 (@y=50, x in [20,140]@) passes
-- through the top vertex of p2 (@(80,50)@) without coinciding with p2's top
-- edge. This is the canonical vertex-on-edge case that requires a 'Boundary'
-- classification in 'pointInPolygonOrBoundary' to select the correct edge
-- fragments.
vertexOnEdgeUnionTest :: TestTree
vertexOnEdgeUnionTest =
    let p1 = boundingBoxPolygon [Vec2 20 50, Vec2 140 120]
        p2 = Polygon [Vec2 60 10, Vec2 100 10, Vec2 80 50]
        result = unionPP p1 p2
    in testVisual "Vertex on edge (union)" 160 140 "docs/geometry/clipping/polygon-polygon-union-vertex-on-edge" $ \_ ->
        polygonBinaryOpRender p1 p2 result

vertexOnEdgeIntersectionTest :: TestTree
vertexOnEdgeIntersectionTest =
    let p1 = boundingBoxPolygon [Vec2 20 50, Vec2 140 120]
        p2 = Polygon [Vec2 60 10, Vec2 100 10, Vec2 80 50]
        result = intersectionPP p1 p2
    in testVisual "Vertex on edge (intersection)" 160 140 "docs/geometry/clipping/polygon-polygon-intersection-vertex-on-edge" $ \_ ->
        polygonBinaryOpRender p1 p2 result

vertexOnEdgeDifferenceTest :: TestTree
vertexOnEdgeDifferenceTest =
    let p1 = boundingBoxPolygon [Vec2 20 50, Vec2 140 120]
        p2 = Polygon [Vec2 60 10, Vec2 100 10, Vec2 80 50]
        result = differencePP p1 p2
    in testVisual "Vertex on edge (difference)" 160 140 "docs/geometry/clipping/polygon-polygon-difference-vertex-on-edge" $ \_ ->
        polygonBinaryOpRender p1 p2 result

-- | Two polygons sharing a full collinear edge segment. p1's right edge
-- (@x=100, y in [10,100]@) and p2's left edge (@x=100, y in [40,100]@) overlap
-- along the segment @(100,40)–(100,100)@. Both endpoints are shared vertices,
-- and the entire overlap is collinear. This stresses both the vertex dedup in
-- 'cutPolygon' and the 'Boundary' midpoint test in 'insertEdgeFragement'.
sharedEdgeUnionTest :: TestTree
sharedEdgeUnionTest =
    let p1 = boundingBoxPolygon [Vec2 10 10, Vec2 100 100]
        p2 = boundingBoxPolygon [Vec2 100 40, Vec2 200 120]
        result = unionPP p1 p2
    in testVisual "Shared collinear edge (union)" 220 140 "docs/geometry/clipping/polygon-polygon-union-shared-edge" $ \_ ->
        polygonBinaryOpRender p1 p2 result

sharedEdgeIntersectionTest :: TestTree
sharedEdgeIntersectionTest =
    let p1 = boundingBoxPolygon [Vec2 10 10, Vec2 100 100]
        p2 = boundingBoxPolygon [Vec2 100 40, Vec2 200 120]
        result = intersectionPP p1 p2
    in testVisual "Shared collinear edge (intersection)" 220 140 "docs/geometry/clipping/polygon-polygon-intersection-shared-edge" $ \_ ->
        polygonBinaryOpRender p1 p2 result

sharedEdgeDifferenceTest :: TestTree
sharedEdgeDifferenceTest =
    let p1 = boundingBoxPolygon [Vec2 10 10, Vec2 100 100]
        p2 = boundingBoxPolygon [Vec2 100 40, Vec2 200 120]
        result = differencePP p1 p2
    in testVisual "Shared collinear edge (difference)" 220 140 "docs/geometry/clipping/polygon-polygon-difference-shared-edge" $ \_ ->
        polygonBinaryOpRender p1 p2 result

failure1 :: TestTree
failure1 =
    let p1 = Polygon
            [ Vec2 (-180.20942060275013) (-29.2891600118938)
            , Vec2 (-104.35744140302432) (-102.09032181668888)
            , Vec2 (-75.65668830760478) 0.0 ]
        p2 = Polygon
            [Vec2 (-180.20942060275013) (-29.2891600118938)
            , Vec2 (-75.65668830760478) 0.0
            , Vec2 (-180.2094206027501) 29.2891600118938 ]
        result = differencePP p1 p2
    in testVisual "Shared edge, FP-divergent shared vertex (difference)" 220 140 "docs/geometry/clipping/polygon-polygon-difference-failure1" $ \_ -> do
        liftIO $ assertEqual "Expected output polygon" (Expected p1) (Actual (fst (head result)))
        polygonBinaryOpRender p1 p2 result

failure2 :: TestTree
failure2 =
    let p1 = Polygon
            [ Vec2 (-180.2094206027501) 29.2891600118938
            , Vec2 (-75.65668830760478) 0.0
            , Vec2 (-94.46830930598787) 163.62391142310275 ]
        p2 = Polygon
            [ Vec2 (-180.20942060275013) (-29.2891600118938)
            , Vec2 (-75.65668830760478) 0.0
            , Vec2 (-180.2094206027501) 29.2891600118938 ]
        result = differencePP p1 p2
    in testVisual "Shared edge added by both polygons (difference)" 220 140 "docs/geometry/clipping/polygon-polygon-difference-failure2" $ \_ -> do
        liftIO $ assertEqual "Expected output polygon" (Expected p1) (Actual (fst (head result)))
        polygonBinaryOpRender p1 p2 result


failure3 :: TestTree
failure3 =
    let p1 = Polygon
            [ Vec2 (-180.2094206027501) 29.2891600118938
            , Vec2 (-75.65668830760478) 0.0
            , Vec2 (-94.46830930598787) 163.62391142310275
            , Vec2 (-180.2094206027501) 29.2891600118938 ]
        p2 = Polygon
            [ Vec2 (-197.119525019955) 0.0
            , Vec2 (-180.20942060275013) (-29.2891600118938)
            , Vec2 (-180.2094206027501) 29.2891600118938 ]
        result = differencePP p1 p2
    in testVisual "Degenerate input (repeated first/last vertex) is sanitized" 220 140 "docs/geometry/clipping/polygon-polygon-difference-failure3" $ \_ ->
        polygonBinaryOpRender p1 p2 result

failure4 :: TestTree
failure4 =
    let p1 = Polygon
            [ Vec2 27.917066630243085 21.213203435596384
            , Vec2 30.251886046322475 23.96972871113259
            , Vec2 30.63132970887029 27.599175236441674
            , Vec2 30.78793750558226 27.339787052829866
            , Vec2 31.163118955396147 29.698484809834987 ]
        p2 = Polygon
            [ Vec2 30.63132970887029 27.599175236441674
            , Vec2 34.4869138749247 21.213203435596384
            , Vec2 38.713868434111056 24.627980214224436 ]
        result = differencePP p1 p2
    in testVisual "Shared vertex with ULP-divergent cuts sorts after snapping" 220 140 "docs/geometry/clipping/polygon-polygon-difference-failure4" $ \_ ->
        polygonBinaryOpRender p1 p2 result

failure5 :: TestTree
failure5 =
    let p1 = Polygon
            [ Vec2 1.3251748478756156 172.00089765020437
            , Vec2 2.5735005625914873 169.57676224990132
            , Vec2 2.650349695751249 169.70562748477138
            , Vec2 23.815850088147716 132.01232159196223
            , Vec2 29.33260934530046 154.30061769351667
            , Vec2 2.650349695751249 169.70562748477138
            , Vec2 29.393459035464485 154.5464569351521
            , Vec2 33.14562986446458 169.70562748477138 ]
        p2 = Polygon
            [ Vec2 (-0.4520657110437618) 170.4886282646589
            , Vec2 1.3251748478756156 172.00089765020437
            , Vec2 2.650349695751249 169.70562748477138 ]
        result = differencePP p1 p2
    in testVisual "Self-touching polygon (repeated non-adjacent vertex)" 220 140 "docs/geometry/clipping/polygon-polygon-difference-failure5" $ \_ ->
        polygonBinaryOpRender p1 p2 result

-- | Regression: the union of two triangles sharing one vertex (and a
-- near-collinear knife edge emanating from it) produced a result polygon with
-- a vertex that lies outside *both* input polygons. The stray vertex
-- @Vec2 (-109.71428571428571) 219.42857142857142@ is the spurious one.
--
-- Every vertex of a correct union must lie in at least one of the input
-- polygons (the union covers only points that are in A or in B), or on its
-- boundary within a small tolerance (output vertices that coincide with input
-- vertices may sit just outside due to FP noise).
--
-- To reproduce: Run current version of marching-cubes showcase, the problematic
-- call ist number 0860 in the debug tracing output
failure6 :: TestTree
failure6 = testCase "Union vertices stay within input polygons (near-collinear shared vertex)" $ do
    let p1 = Polygon
            [ Vec2 (-197.28435427052983) 2.29527016543296
            , Vec2 (-148.2945594148663) (-82.55754357695274)
            , Vec2 (-149.61973426274193) 84.8528137423857 ]
        p2 = Polygon
            [ Vec2 (-149.61973426274193) 84.8528137423857
            , Vec2 (-147.42145027803443) 85.6358145222732
            , Vec2 (-100.62993940707837) 169.70562748477138 ]
        result = unionPP p1 p2
        allOutputCorners = concatMap (\(Polygon ps, _) -> ps) result
        -- A point is "in or on" a polygon if it is inside, or close to an
        -- edge (within 1e-6). Output vertices that coincide with input
        -- vertices may sit a few ULPs outside; a genuine stray vertex like
        -- @(-109.7, 219.4)@ is ~30 units away from every edge.
        inOrOn p poly = pointInPolygon p poly
            || any (<= 1e-6) [distanceFromLine p edge | edge <- polygonEdges poly]
        outsideBoth p = not (inOrOn p p1) && not (inOrOn p p2)
        bad = filter outsideBoth allOutputCorners
    unless (null bad) $ assertFailure (unlines
        $ "Union produced vertices outside both input polygons:"
        : map (("  " ++) . show) bad)
